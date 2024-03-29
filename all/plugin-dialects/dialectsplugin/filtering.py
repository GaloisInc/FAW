from typing import Literal

import numpy as np
import numpy.typing as npt

from dialectsplugin.similarity import file_distribution_attributable_risk

TargetRestrictionMode = Literal["target_only", "homogeneous_outside", "ignore_outside"]


def select_valid_heroes(
    *,
    feature_files: npt.NDArray[np.bool_],
    target_files: npt.NDArray[np.int_],
    min_dialect_size: int,
    feature_slop: npt.NDArray[np.int_],
    excluded_features: npt.NDArray[np.int_],
    exclusion_min_attr_risk: float,
    max_slop_files: int,
) -> npt.NDArray[np.int_]:
    """Return the indices of features which are valid candidates for dialect heroes.

    min_dialect_size is the min # of positive and negative samples _within_
    the target required for consideration
    """
    # min_dialect_size = max(min_dialect_size, max_slop_files)
    candidate_heroes_mask = np.ones(feature_files.shape[0], dtype=np.bool_)

    feature_counts: npt.NDArray[np.int_] = np.sum(
        feature_files[:, target_files], axis=1
    )
    candidate_heroes_mask &= feature_slop <= max_slop_files
    candidate_heroes_mask &= feature_counts >= min_dialect_size
    # We also need enough space to fit at least one more valid dialect in
    candidate_heroes_mask &= feature_counts + min_dialect_size <= len(target_files)

    for feature in excluded_features:
        # TODO should this be restricted to the target?
        similarities = file_distribution_attributable_risk(
            feature_files[feature, :],
            feature_files,
        )
        candidate_heroes_mask &= similarities < exclusion_min_attr_risk

    return np.nonzero(candidate_heroes_mask)[0]


def deduplicated_feature_indices(
    feature_files: npt.NDArray[np.bool_],
    *,
    target_files: npt.NDArray[np.int_],
    feature_slop: npt.NDArray[np.int_],
) -> tuple[npt.NDArray[np.int_], npt.NDArray[np.int_]]:
    """Return indices of unique features, by file distribution,
    and return the index indices into the unique features that
    can be used to reconstruct feature_files[:, target_files].

    Features with the same file distribution within the target are removed, keeping
    the dupe with the least slop contribution.
    """
    target_feature_files = feature_files[:, target_files]  # deduplicate by this
    slop_priority_indices = np.argsort(feature_slop)  # prioritize by this

    _, unique_features_sorted, unique_features_sorted_inverse = np.unique(
        # sorted by slop (lowest first)
        target_feature_files[slop_priority_indices, :],
        axis=0,
        return_index=True,
        return_inverse=True,
    )
    # convert those indices back to indices into target_feature_files
    # (i.e. undo the sort by slop)
    return (
        slop_priority_indices[unique_features_sorted],
        unique_features_sorted_inverse[np.argsort(slop_priority_indices)],
    )


def feature_slop(
    feature_files: npt.NDArray[np.bool_],
    *,
    target_files: npt.NDArray[np.int_],
    target_restriction_mode: TargetRestrictionMode,
) -> npt.NDArray[np.int_]:
    if target_restriction_mode == "target_only":
        return np.sum(
            np.delete(feature_files, target_files, axis=1),
            axis=1,
        )
    elif target_restriction_mode == "homogeneous_outside":
        feature_counts_outside_target = np.sum(
            np.delete(feature_files, target_files, axis=1),
            axis=1,
        )
        return np.minimum(
            feature_counts_outside_target,
            feature_files.shape[1] - target_files.size - feature_counts_outside_target,
        )
    else:
        return np.zeros(feature_files.shape[0], dtype=int)
