import collections
import itertools
import logging
from typing import Optional, Sequence, List, Tuple

import numpy as np
import numpy.typing as npt

from partitioninginterface import PartitioningSettings, PartitioningAlgorithm, Dialect


class ExhaustivePartitioning(PartitioningAlgorithm):

    @staticmethod
    def dialects_of(
        *,
        target_feature_indices: Optional[Sequence[int]],
        file_features: npt.NDArray[np.bool_],  # features x files; may be sparse
        settings: PartitioningSettings,
    ) -> Sequence[Sequence[Dialect]]:  # return type doesn't include info like entropy, since we can compute that later
        # Just brute force it and find all exact partitions. This will be slow

        # TODO maybe these first chunks should be factored out, since it'll be the same for all algorithms
        feature_counts = file_features.sum(1)
        target_file_mask: npt.NDArray[np.bool_]
        target_files: npt.NDArray[np.int_]
        if target_feature_indices is None:
            # Target is all files
            target_file_mask = np.ones(file_features.shape[1], dtype=bool)
            target_files = np.arange(file_features.shape[1])
        else:
            feature_counts[target_feature_indices] = 0  # Make target unavailable as dialects
            # must reshape since scipy sparse behaves weirdly under np.any w.r.t. dims
            target_file_mask = np.any(file_features[target_feature_indices, :], axis=0).todense().reshape((file_features.shape[1],))
            target_files = target_file_mask.nonzero()[0]

        # Feature indices available as dialects
        candidate_features_mask: npt.NDArray[np.bool_] = feature_counts >= settings.min_feature_samples
        # drop features that aren't represented within the target
        candidate_features_mask &= file_features[:, target_files].sum(1) > 0
        if settings.restrict_to_target:
            # Drop features (incl. negated pseudofeatures) that are represented out of the target
            # TODO allow some slop if configured to
            candidate_features_mask &= file_features[:, (~target_file_mask).nonzero()[0]].sum(1) == 0

        ##### approach-specific logic below

        # out-of-bounds features are negated
        candidate_features: npt.NDArray[np.int_] = (
            candidate_features_mask
        ).nonzero()[0]

        # Brute force approach; *very* inefficient

        partitions: List[List[Dialect]] = []
        for subset_indices in itertools.chain.from_iterable(
            itertools.combinations(range(len(candidate_features)), r)
            for r in range(2, (len(candidate_features) // settings.min_feature_samples) + 1)
        ):
            subset_indices = list(subset_indices)
            subset = candidate_features[subset_indices]

            # TODO remove this condition
            # for now, stop after we find 6 partitions
            if len(partitions) >= 6:
                break
            if np.all(file_features[subset, :][:, target_files].sum(0) == 1):
                partitions.append([Dialect(i, False) for i in subset])

        return partitions
    
        # a depth-first search would be a maybe more efficient option here


    # UNUSED for now
    @staticmethod
    def dialects_of_with_negation(
        *,
        target_feature_indices: Optional[Sequence[int]],
        file_features: npt.NDArray[np.bool_],  # features x files; may be sparse
        settings: PartitioningSettings,
    ) -> Sequence[Sequence[Dialect]]:  # return type doesn't include info like entropy, since we can compute that later
        # Just brute force it and find all exact partitions. This will be slow

        # TODO maybe these first chunks should be factored out, since it'll be the same for all algorithms
        feature_counts = file_features.sum(1)
        target_file_mask: npt.NDArray[np.bool_]
        target_files: npt.NDArray[np.int_]
        if target_feature_indices is None:
            # Target is all files
            target_file_mask = np.ones(file_features.shape[1], dtype=bool)
            target_files = np.arange(file_features.shape[1])
        else:
            feature_counts[target_feature_indices] = 0  # Make target unavailable as dialects
            # must reshape since scipy sparse behaves weirdly under np.any w.r.t. dims
            target_file_mask = np.any(file_features[target_feature_indices, :], axis=0).todense().reshape((file_features.shape[1],))
            target_files = target_file_mask.nonzero()[0]

        # Feature indices available as dialects
        # Features may be available directly, negated, or both
        candidate_positive_features_mask: npt.NDArray[np.bool_] = feature_counts >= settings.min_feature_samples
        candidate_negated_features_mask: npt.NDArray[np.bool_] = file_features.shape[1] - feature_counts >= settings.min_feature_samples
        # drop features that aren't represented within the target
        candidate_positive_features_mask &= file_features[:, target_files].sum(1) > 0
        candidate_negated_features_mask &= file_features[:, target_files].sum(1) < target_files.size
        if settings.restrict_to_target:
            # Drop features (incl. negated pseudofeatures) that are represented out of the target
            # TODO allow some slop if configured to
            candidate_positive_features_mask &= file_features[:, (~target_file_mask).nonzero()[0]].sum(1) == 0
            candidate_negated_features_mask &= file_features[:, target_files].sum(1) == 0

        ##### approach-specific logic below

        # out-of-bounds features are negated
        candidate_features: npt.NDArray[np.int_] = (
            candidate_positive_features_mask
            | candidate_negated_features_mask
        ).nonzero()[0]
        candidate_features_positive_ok: npt.NDArray[np.bool_] = candidate_positive_features_mask[candidate_features]
        candidate_features_negated_ok: npt.NDArray[np.bool_] = candidate_negated_features_mask[candidate_features]

        # Brute force approach; *very* inefficient

        partitions: List[List[Dialect]] = []
        for subset_indices in itertools.chain.from_iterable(
            itertools.combinations(range(len(candidate_features)), r)
            for r in range(2, (len(candidate_features) // settings.min_feature_samples) + 1)
        ):
            subset_indices = list(subset_indices)
            subset = candidate_features[subset_indices]

            # pick pos/neg for each candidate feature
            # feature_negated is parallel to subset
            for feature_negated in itertools.product(*(
                col.nonzero()[0]
                for col in np.vstack([
                    candidate_features_positive_ok[subset_indices],
                    candidate_features_negated_ok[subset_indices],
                ]).T
            )):
                # TODO remove this condition
                # for now, stop after we find 4 partitions
                if len(partitions) >= 6:
                    break
                positive_subset = [i for i, negated in zip(subset, feature_negated) if not negated]
                negated_subset = [i for i, negated in zip(subset, feature_negated) if negated]
                # TODO this is extra inefficient since it drops sparsity
                if np.all(
                    file_features[positive_subset, :][:, target_files].sum(0)
                    + (~file_features[negated_subset, :][:, target_files].todense()).sum(0)
                    == 1
                ):
                    partitions.append(
                        [
                            *(Dialect(i, False) for i in positive_subset),
                            *(Dialect(i, True) for i in negated_subset),
                        ]
                    )
            # if np.all(file_features[subset, :][:, target_files].sum(0) == 1):
            #     partitions.append(subset)

        return partitions

        # # find non-overlapping sets of features
        # # depth-first search
        # dialects = []

        # for i, feature in enumerate(candidate_features):

