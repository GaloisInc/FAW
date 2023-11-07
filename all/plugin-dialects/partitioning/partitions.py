import dataclasses
from typing import List, Mapping, Optional, Sequence, Tuple

import numpy as np
import numpy.typing as npt
import scipy.spatial
import scipy.special


from partitioning.filtering import TargetRestrictionMode, select_valid_heroes
from partitioning.similarity import bernoulli_entropy_weights, pairwise_attributable_risk


def best_partitions(
    feature_files: npt.NDArray[np.bool_],
    *,
    target_features_cnf: Mapping[int, bool],
    min_feature_samples: int,
    target_restriction_mode: TargetRestrictionMode,
    max_slop_files: int,
    max_dialects: int = 3,
    excluded_features: npt.NDArray[np.int_],
    exclusion_min_attr_risk: float,
    feature_names: Sequence[str],  # TODO remove
# ) -> Sequence[Sequence[int]]:  # feature indices in feature_files
) -> Tuple[Sequence[Sequence[int]], List[str]]:  # feature indices in feature_files

    target_files = target_files_from_cnf(feature_files, target_features_cnf)

    # Drop exact duplicate rows, with indices to reconstruct the original
    # TODO should consider features dupes when they coincide exactly
    #   within the target, but not outside it. For now we don't.
    #   Would need to keep the dupes with the least out-of-target representation
    feature_files, unique_feature_indices, unique_inverse_indices = np.unique(
        feature_files, axis=0, return_index=True, return_inverse=True
    )
    target_feature_files = feature_files[:, target_files]

    valid_heroes = select_valid_heroes(
        feature_files,
        target_files=target_files,
        min_feature_samples=max(min_feature_samples, max_slop_files),
        target_restriction_mode=target_restriction_mode,
        excluded_features=unique_inverse_indices[excluded_features],
        exclusion_min_attr_risk=exclusion_min_attr_risk,
        max_slop_files=max_slop_files,
    )
    if len(valid_heroes) == 0:
        return [], ['no candidate hero features', str(target_files)]
    num_files_with_candidate_feature_outside_target = np.sum(
        np.delete(feature_files[valid_heroes, :], target_files, axis=1),
        axis=1,
    )
    partitions, debug_lines = _find_partition_heroes(
        valid_heroes=valid_heroes,
        target_feature_files=target_feature_files,
        hero_slop=num_files_with_candidate_feature_outside_target,
        max_slop_files=max_slop_files,
        max_dialects=max_dialects,
        max_partitions=10,  # TODO unhardcode
        hero_names=np.array(feature_names)[unique_feature_indices][valid_heroes]
    )

    # transform back to indices in the original, pre-deduplication feature_files
    return [unique_feature_indices[partition] for partition in partitions], debug_lines


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
    partition: List[int]
    """Hero indices (not feature indices)"""
    last_contribution: Optional[int]
    """Coverage of last feature added to partition (coverage = #files - slop)"""


def _find_partition_heroes(
    *,
    valid_heroes: npt.NDArray[np.int_],
    target_feature_files: npt.NDArray[np.bool_],
    hero_slop: npt.NDArray[np.int_],
    max_slop_files: int,
    max_dialects,
    max_partitions,
    hero_names: Sequence[str]  # TODO remove
) -> Tuple[List[List[int]], List[str]]:
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
    # TODO is the entropy reasonable as a factor here?
    # TODO should we weight by feature rarity as well?
    # TODO is logsumexp 
    hero_weights = scipy.special.logsumexp(
        # Take absolute value, since anti-attribution should also count here
        np.abs(pairwise_attr_risk), axis=1,
        b=(1 / pairwise_attr_risk.shape[1]),  # noramlizing factor (multiplied in here instead of subtracted after the log for better numeric stability)
    ) * hero_feature_entropy
    hero_weights = np.mean(
        np.abs(pairwise_attr_risk), axis=1,
    ) * hero_feature_entropy

    incomplete_partition_stack: List[_PartialPartition] = [_PartialPartition([], None)]
    good_partitions: List[List[int]] = []
    while incomplete_partition_stack and len(good_partitions) < max_partitions:
        # Remove the most recently inserted item
        prior = incomplete_partition_stack.pop()
        seed_partition = prior.partition
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
        hero_overlap_contribution = np.sum(
            hero_feature_files[:, covered_files], axis=1
        )
        hero_slop_contribution = hero_slop + hero_overlap_contribution
        candidate_heroes_mask = hero_slop_contribution <= allowable_slop
        candidate_heroes_mask &= (
            (hero_file_counts - hero_slop_contribution) / hero_file_counts
            >= 0.5
        )
        hero_additional_coverage = np.sum(
            hero_feature_files[:, remaining_files], axis=1
        )
        candidate_heroes_mask &= (
            hero_additional_coverage * (max_dialects - len(seed_partition))
            >= num_target_files - num_covered_files
        )
        if prior.last_contribution is not None:
            candidate_heroes_mask &= (
                hero_additional_coverage <= prior.last_contribution
            )
        candidate_heroes = np.nonzero(candidate_heroes_mask)[0]
        num_candidates = len(candidate_heroes)
        if num_candidates == 0:
            continue

        candidate_file_features = hero_feature_files[np.ix_(candidate_heroes, ~partition_mask)]
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
            * ((allowable_slop - candidate_slop_contribution) / allowable_slop)  # between 0 and 1; fraction of slop that will remain available
            * candidate_rarity_satisfaction  # between 0 and 1
        )
        # TODO check if this sort is too slow....
        # Sort ascending, since we want to extend the stack in ascending order (last item is popped)
        candidate_sort_indices = np.argsort(candidate_weights, kind="stable")#[::-1]

        incomplete_partition_stack.extend(
            _PartialPartition(
                partition=seed_partition + [candidate_i],
                last_contribution=hero_additional_coverage[candidate_i],
            )
            for candidate_i in candidate_heroes[candidate_sort_indices]
        )

    # Convert hero indices to feature indices and return
    return [
        valid_heroes[partition] for partition in good_partitions
    ], []


# TODO remove if unused
def screen(a, b):
    # `screen` is a method of aggregating values between 0 and 1
    # that always returns a greater value than both inputs,
    # as opposed to multiplication, that returns a lesser value.
    # Traditionally this is used as an image blend mode in photo editing software.
    return (1 - ((1 - a) * (1 - b)))


def screen_array(arr: npt.NDArray[np.float_], axis) -> npt.NDArray[np.float_]:
    return 1 - np.product(1 - arr, axis=axis)
