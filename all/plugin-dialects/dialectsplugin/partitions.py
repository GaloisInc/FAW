import dataclasses
from typing import Generic, List, Mapping, Optional, Sequence, Set, TypeVar

import numpy as np
import numpy.typing as npt
import scipy.spatial
import scipy.special

from dialectsplugin.filtering import (
    TargetRestrictionMode,
    deduplicated_feature_indices,
    feature_slop,
    select_valid_heroes,
)
from dialectsplugin.similarity import (
    bernoulli_entropy_weights,
    pairwise_attributable_risk,
)

T = TypeVar("T")


@dataclasses.dataclass
class WithDebugLines(Generic[T]):
    value: T
    debug_lines: Sequence[str]


def best_partitions(
    feature_files: npt.NDArray[np.bool_],
    *,
    target_features_cnf: Mapping[int, bool],
    min_feature_samples: int,
    target_restriction_mode: TargetRestrictionMode,
    max_slop_files: int,
    max_dialects: int,
    max_partitions: int,
    no_partition_overlap: bool,
    excluded_features: npt.NDArray[np.int_],
    exclusion_min_attr_risk: float,
    feature_names: Sequence[str],
) -> WithDebugLines[Sequence[npt.NDArray[np.int_]]]:  # feature indices in feature_files
    target_files = target_files_from_cnf(feature_files, target_features_cnf)

    feature_slop_ = feature_slop(
        feature_files=feature_files,
        target_restriction_mode=target_restriction_mode,
        target_files=target_files,
    )

    unique_features = deduplicated_feature_indices(
        feature_files=feature_files,
        target_files=target_files,
        feature_slop=feature_slop_,
    )
    unique_feature_files = feature_files[unique_features, :]
    unique_feature_slop = feature_slop_[unique_features]
    target_feature_files = feature_files[np.ix_(unique_features, target_files)]

    # These hero indices are indices into unique_feature_files
    valid_heroes = select_valid_heroes(
        unique_feature_files,
        target_files=target_files,
        min_feature_samples=max(min_feature_samples, max_slop_files),
        feature_slop=unique_feature_slop,
        excluded_features=excluded_features,
        exclusion_min_attr_risk=exclusion_min_attr_risk,
        max_slop_files=max_slop_files,
    )
    if len(valid_heroes) == 0:
        return WithDebugLines([], ["no candidate hero features", str(target_files)])
    hero_slop = unique_feature_slop[valid_heroes]

    partitions_with_debug_lines = _find_partition_heroes(
        valid_heroes=valid_heroes,
        target_feature_files=target_feature_files,
        hero_slop=hero_slop,
        max_slop_files=max_slop_files,
        max_dialects=max_dialects,
        max_partitions=max_partitions,
        exclusion_min_attr_risk=exclusion_min_attr_risk,
        no_partition_overlap=no_partition_overlap,
        hero_names=np.array(feature_names)[unique_features][valid_heroes],
    )

    # transform back to indices in the original, pre-deduplication feature_files
    return WithDebugLines(
        [unique_features[partition] for partition in partitions_with_debug_lines.value],
        partitions_with_debug_lines.debug_lines,
    )


def target_files_from_cnf(
    feature_files: npt.NDArray[np.bool_],
    target_features_cnf: Mapping[int, bool],
) -> npt.NDArray[np.int_]:
    if not target_features_cnf:
        return np.arange(feature_files.shape[1])
    target_files_mask = np.ones(feature_files.shape[1], dtype=np.bool_)
    for feature, pos in target_features_cnf.items():
        target_files_mask &= feature_files[feature, :] == pos
    return np.nonzero(target_files_mask)[0]


@dataclasses.dataclass
class _PartialPartition:
    heroes: List[int]
    """Hero indices (not feature indices)"""
    last_contribution: Optional[int]
    """Coverage of last feature added to partition (coverage = #files - slop)"""


def _find_partition_heroes(
    *,
    valid_heroes: npt.NDArray[np.int_],
    target_feature_files: npt.NDArray[np.bool_],
    hero_slop: npt.NDArray[np.int_],
    max_slop_files: int,
    max_dialects: int,
    max_partitions: int,
    no_partition_overlap: bool,
    exclusion_min_attr_risk: float,  # only used when no_partition_overlap
    hero_names: Sequence[str],
) -> WithDebugLines[List[List[int]]]:
    # New version of partition search, without recursion
    num_target_files = target_feature_files.shape[1]

    hero_feature_files = target_feature_files[valid_heroes, :]
    hero_file_counts = np.sum(hero_feature_files, axis=1)
    # TODO return to this approaich
    pairwise_attr_risk = pairwise_attributable_risk(
        hero_feature_files,
        target_feature_files,
    )
    # file_entropy = bernoulli_entropy_weights(
    #     counts=np.sum(target_feature_files, axis=0),
    #     total=target_feature_files.shape[0],
    # )
    hero_feature_entropy = bernoulli_entropy_weights(
        counts=hero_file_counts,
        total=num_target_files,
    )
    # TODO should we weight by file rarity as well?
    hero_weights = (
        scipy.special.logsumexp(
            # Take absolute value, since anti-attribution should also count here
            np.abs(pairwise_attr_risk),
            axis=1,
            # noramlizing factor (multiplied in here instead of subtracted after
            # the log for better numeric stability)
            b=(1 / pairwise_attr_risk.shape[1]),
        )
        * hero_feature_entropy
    )

    incomplete_partition_stack: List[_PartialPartition] = [_PartialPartition([], None)]
    good_partitions: List[List[int]] = []
    avoiding_features_mask = np.zeros_like(valid_heroes, dtype=np.bool_)
    while incomplete_partition_stack and len(good_partitions) < max_partitions:
        # Remove the most recently inserted item
        prior = incomplete_partition_stack.pop()
        seed_partition = prior.heroes
        seed_partition_features = valid_heroes[seed_partition]

        partition_sum = np.sum(target_feature_files[seed_partition_features, :], axis=0)
        partition_mask = partition_sum > 0
        partition_overlap = np.sum(partition_sum > 1)
        num_covered_files = np.sum(partition_mask)

        allowable_slop = max_slop_files - partition_overlap

        # Check if seed already is good
        if (
            num_target_files - num_covered_files <= allowable_slop
            and len(seed_partition) >= 2
        ):
            # Done; success
            good_partitions.append(seed_partition)
            if no_partition_overlap:
                # Remove any similar features from future consideration
                similar_features_mask = np.any(
                    pairwise_attr_risk[np.ix_(seed_partition, valid_heroes)]
                    >= exclusion_min_attr_risk,
                    axis=0,
                )
                avoiding_features_mask |= similar_features_mask
                incomplete_partition_stack = [
                    partial_partition
                    for partial_partition in incomplete_partition_stack
                    if not np.any(avoiding_features_mask[partial_partition.heroes])
                ]
            continue
        elif len(seed_partition) == max_dialects:
            # Can't add more dialects
            continue

        # Now we find valid children of this partition and enstack them
        covered_files = np.nonzero(partition_mask)[0]
        remaining_files = np.nonzero(~partition_mask)[0]

        # TODO cache some of these if necessary
        # Remove from consideration:
        # - features which would contribute too much slop
        # - features that are less than half in the remaining files
        #   - includes features already in partition
        # - very small features, smaller than one equal part of
        #   the remaining files if all allowable dialects spent
        #   - This makes a huge difference to perf
        # - Features contributing less than the previous feature in the
        #   partition, so we always build partitions starting with the biggest
        #   features (and avoid blowing up the stack with duplicates)
        hero_overlap_contribution = np.sum(hero_feature_files[:, covered_files], axis=1)
        # Note: This hero slop contribution is only an upper bound--a file out of
        # the target but in multiple dialects contributes extra slop for each
        # dialect. This isn't the correct logic, but it's faster this way.
        # TODO try the real logic here and see if it's faster
        hero_slop_contribution = hero_slop + hero_overlap_contribution
        candidate_heroes_mask = hero_slop_contribution <= allowable_slop
        candidate_heroes_mask &= (
            hero_file_counts - hero_slop_contribution
        ) / hero_file_counts >= 0.5
        hero_additional_coverage = np.sum(
            hero_feature_files[:, remaining_files], axis=1
        )
        candidate_heroes_mask &= (
            hero_additional_coverage * (max_dialects - len(seed_partition))
            >= num_target_files - num_covered_files
        )
        if prior.last_contribution is not None:
            candidate_heroes_mask &= hero_additional_coverage <= prior.last_contribution
        if no_partition_overlap:
            candidate_heroes_mask &= ~avoiding_features_mask
        candidate_heroes = np.nonzero(candidate_heroes_mask)[0]
        num_candidates = len(candidate_heroes)
        if num_candidates == 0:
            continue

        candidate_file_features = hero_feature_files[
            np.ix_(candidate_heroes, ~partition_mask)
        ]
        coverage_by_candidates = np.sum(candidate_file_features, axis=0)
        if (
            coverage_by_candidates.size - np.count_nonzero(coverage_by_candidates)
            > allowable_slop
        ):
            # There are too many unreachable files
            # TODO when there are _almost_ too many unreachable files, prune more heavily, considering the max partition size
            continue

        # Prefer:
        # - features that cover rare files we don't have yet
        # - features that contribute less slop
        # - features with high weight

        # file rarity: 1 when a file is only in 1 file, 0 when it's in all files
        # that is, the number of *other* features that don't have the file, for a feature with it.
        file_rarity = (num_candidates - coverage_by_candidates) / (num_candidates - 1)
        candidate_rarity_satisfaction = scipy.special.logsumexp(
            candidate_file_features * file_rarity,
            axis=1,
            b=(1 / file_rarity.size),
        )
        candidate_slop_contribution = hero_slop_contribution[candidate_heroes]
        candidate_weights = (
            hero_weights[candidate_heroes]  # between 0 and 1
            + (
                (allowable_slop - candidate_slop_contribution) / allowable_slop
            )  # between 0 and 1; fraction of slop that will remain available
            + candidate_rarity_satisfaction  # between 0 and 1
        )
        # Sort ascending, since we want to extend the stack in ascending order (last item is popped)
        candidate_sort_indices = np.argsort(candidate_weights, kind="stable")

        incomplete_partition_stack.extend(
            _PartialPartition(
                heroes=seed_partition + [candidate_i],
                last_contribution=hero_additional_coverage[candidate_i],
            )
            for candidate_i in candidate_heroes[candidate_sort_indices]
        )

    # Convert hero indices to feature indices and return
    return WithDebugLines(
        [valid_heroes[partition] for partition in good_partitions],
        [],
    )
