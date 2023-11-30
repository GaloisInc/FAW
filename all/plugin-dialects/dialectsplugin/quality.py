from typing import Callable, Collection, Dict, Sequence

import numpy as np
import numpy.typing as npt
import scipy.stats


def dialect_size_entropy_partition_quality(
    partition: Collection[Sequence[int]],
    file_features: npt.NDArray[np.bool_],
) -> float:
    dialect_sizes = [len(dialect) for dialect in partition]
    entropy = scipy.stats.entropy(dialect_sizes)
    # higher entropy is better; means dialects are more even
    return entropy


def min_cosine_distance_partition_quality(
    partition: Collection[Sequence[int]],
    file_features: npt.NDArray[np.bool_],
) -> float:
    # A proxy for a measure of how redundant partitions are with each other
    # High redundancy = partitions should be merged
    # Low reduncancy = we have minimal partitions
    #
    # We want to see different proportions of each feature in each dialect.

    # Method: We find redundancy per-feature
    # The idea is that a redundant partition is one that is highly redundant for all features

    # rows: feature
    # columns: partition
    # values: proportion of files in dialect w/ feature
    table = np.zeros((file_features.shape[0], len(partition)), dtype=np.float_)
    for i_dialect, dialect in enumerate(partition):
        table[:, i_dialect] = np.mean(
            file_features[:, dialect], axis=1
        )  # mean since we care about proportions

    # Now, we find the minimum cosine dist between columns
    # There is a faster vectorized way to compute this
    min_cos_dist = np.inf
    for i in range(len(partition)):
        for j in range(i + 1, len(partition)):
            min_cos_dist = min(
                min_cos_dist, scipy.spatial.distance.cosine(table[:, i], table[:, j])
            )
    return min_cos_dist


def min_distance_partition_quality(
    partition: Collection[Sequence[int]],
    file_features: npt.NDArray[np.bool_],
    *,
    distance_metric: Callable,  # scipy distance metric function or equivalent
) -> float:
    # generic version of the above
    # rows: feature
    # columns: partition
    # values: proportion of files in dialect w/ feature
    table = np.zeros((file_features.shape[0], len(partition)), dtype=np.float_)
    for i_dialect, dialect in enumerate(partition):
        table[:, i_dialect] = np.mean(
            file_features[:, dialect], axis=1
        )  # mean since we care about proportions

    # Now, we find the minimum dist between columns
    # There is a faster vectorized way to compute this
    min_dist = np.inf
    for i in range(len(partition)):
        for j in range(i + 1, len(partition)):
            min_dist = min(min_dist, distance_metric(table[:, i], table[:, j]))
    return min_dist


def min_cosine_distance_with_inversion_partition_quality(
    partition: Collection[Sequence[int]],
    file_features: npt.NDArray[np.bool_],
) -> float:
    # rows: feature
    # columns: partition
    # values: proportion of files in dialect w/ feature
    table = np.zeros((file_features.shape[0], len(partition)), dtype=np.float_)
    for i_dialect, dialect in enumerate(partition):
        table[:, i_dialect] = np.mean(
            file_features[:, dialect], axis=1
        )  # mean since we care about proportions

    # Now, for each cell, invert about 0.5 if greater than 0.5
    table = np.minimum(table, 1 - table)

    # Now, we find the minimum cosine dist between columns
    # There is a faster vectorized way to compute this
    min_cos_dist = np.inf
    for i in range(len(partition)):
        for j in range(i + 1, len(partition)):
            min_cos_dist = min(
                min_cos_dist, scipy.spatial.distance.cosine(table[:, i], table[:, j])
            )
    return min_cos_dist


def dialect_cosine_distance_partition_quality(
    partition: Collection[Sequence[int]],
    file_features: npt.NDArray[np.bool_],
) -> float:
    # A proxy for a measure of how redundant partitions are with each other
    # High redundancy = partitions should be merged
    # Low reduncancy = we have minimal partitions

    # Method: We find redundancy per-feature
    # The idea is that a redundant partition is one that is highly redundant for all features

    # rows: feature
    # columns: partition
    # values: proportion of files in dialect w/ feature
    table = np.zeros((file_features.shape[0], len(partition)), dtype=np.float_)
    for i_dialect, dialect in enumerate(partition):
        table[:, i_dialect] = np.mean(
            file_features[:, dialect], axis=1
        )  # mean since we care about proportions

    # Now we want the average similarity between the columns

    # Method 1: average pairwise distances
    # Upper triangular matrix; all other cells are left as zeros
    distance_matrix = np.zeros((len(partition), len(partition)), dtype=float)
    for i in range(len(partition)):
        for j in range(i + 1, len(partition)):
            # TODO is this the right distance metric
            # distance_matrix[i, j] = scipy.spatial.distance.cityblock(table[:, i], table[:, j])
            distance_matrix[i, j] = scipy.spatial.distance.cosine(
                table[:, i], table[:, j]
            )
    # Only average the cells we filled
    return np.mean(distance_matrix[np.triu_indices_from(distance_matrix)])


def hamming_distance_partition_quality(
    partition: Collection[Sequence[int]],
    file_features: npt.NDArray[np.bool_],
) -> float:
    # Simple metric based on hamming distance.
    # This probably isn't what we want to use, since the aggregation isn't natural for >2 dialects

    # For each dialect, we compute its hamming distance to each feature.
    # We then average the distances for each feature.
    # Then we average those averages.
    dialect_distances = []
    for dialect in partition:
        dialect_mask = np.zeros(file_features.shape[1], dtype=bool)
        dialect_mask[dialect] = True
        # We compute similarity here instead of difference
        distances = np.mean(file_features == dialect_mask, axis=1)
        dialect_distances.append(distances)
    return np.mean(np.column_stack(dialect_distances))


def hamming_correlation_partition_quality(
    partition: Collection[Sequence[int]],
    file_features: npt.NDArray[np.bool_],
) -> float:
    # Simple metric based on hamming distance, but symmetrical, so a dialect being close to a feature's
    # complement is as good as being close to the feature itself.
    # This probably isn't what we want to use, since the aggregation isn't natural for >2 dialects

    # For each dialect, we compute its correlation to each features, then abs it.
    # We then average the distances for each feature.
    # Then we average those averages.
    dialect_distances = []
    for dialect in partition:
        dialect_mask = np.zeros(file_features.shape[1], dtype=bool)
        dialect_mask[dialect] = True
        distances = np.abs(1 - 2 * np.mean(file_features == dialect_mask, axis=1))
        dialect_distances.append(distances)
    # Higher correlation is better
    return np.mean(np.column_stack(dialect_distances))


quality_metrics: Dict[
    str, Callable[[Collection[Sequence[int]], npt.NDArray[np.bool_]], float]
] = {
    "Size Entropy": dialect_size_entropy_partition_quality,
    "Hamming Correlation": hamming_correlation_partition_quality,
    "Hamming Distance": hamming_distance_partition_quality,
    "Cosine Distance": dialect_cosine_distance_partition_quality,
    "Minimum Cosine Distance": min_cosine_distance_partition_quality,
    "Minimum Cosine Distance With Inversion": min_cosine_distance_with_inversion_partition_quality,
}
