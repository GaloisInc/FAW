from typing import Literal

import numpy as np
import numpy.typing as npt

from partitioning.similarity import file_distribution_attributable_risk


TargetRestrictionMode = Literal['target_only', 'homogeneous_outside', 'ignore_outside']


def select_valid_heroes(
    feature_files: npt.NDArray[np.bool_],
    *,
    target_files: npt.NDArray[np.int_],
    min_feature_samples: int,
    target_restriction_mode: TargetRestrictionMode,
    excluded_features: npt.NDArray[np.int_],
    exclusion_min_attr_risk: float,
    max_slop_files: int,
) -> npt.NDArray[np.int_]:
    """Return the indices of features which are valid candidates for dialect heroes.

    min_feature_samples is the min # of positive and negative samples _within_
    the target required for consideration
    """
    candidate_heroes_mask = np.ones(feature_files.shape[0], dtype=np.bool_)
    feature_counts = np.sum(feature_files[:, target_files], axis=1)
    if target_restriction_mode == 'target_only':
        candidate_heroes_mask &= np.sum(
            np.delete(feature_files, target_files, axis=1),
            axis=1,
        ) < max_slop_files
    elif target_restriction_mode == 'homogeneous_outside':
        feature_counts_outside_target = np.sum(
            np.delete(feature_files, target_files, axis=1),
            axis=1,
        )
        candidate_heroes_mask &= np.minimum(
            feature_counts_outside_target,
            feature_files.shape[1] - target_files.size - feature_counts_outside_target,
        ) < max_slop_files
    candidate_heroes_mask &= feature_counts >= min_feature_samples
    candidate_heroes_mask &= feature_counts <= len(target_files) - min_feature_samples

    for feature in excluded_features:
        # TODO should this be restricted to the target?
        similarities = file_distribution_attributable_risk(
            feature_files[feature, :],
            feature_files,
        )
        candidate_heroes_mask &= similarities < exclusion_min_attr_risk
    return np.nonzero(candidate_heroes_mask)[0]
