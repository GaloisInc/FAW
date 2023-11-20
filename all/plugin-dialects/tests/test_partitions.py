import numpy as np

from dialectsplugin.filtering import deduplicated_feature_indices
from dialectsplugin.partitions import _InversionInfo


def test__InversionInfo_from_unique_features():
    rng = np.random.default_rng(seed=0)
    # More features than files, so we're pigeonholed into seeing duplicates
    n_features = 100
    n_files = 10
    target_files = np.arange(4)
    feature_files = rng.integers(0, 2, (n_features, n_files), dtype=np.bool_)

    first_inverted_feature = n_features
    feature_files = np.concatenate([feature_files, ~feature_files])

    feature_slop = rng.random(n_features * 2)
    unique_features, unique_features_indices = deduplicated_feature_indices(
        feature_files=feature_files,
        target_files=target_files,
        feature_slop=feature_slop,
    )

    inversion_info = _InversionInfo.from_unique_features(
        original_first_inverted_feature=first_inverted_feature,
        unique_features=unique_features,
        unique_features_inverse=unique_features_indices,
    )

    assert (
        inversion_info.feature_direct_inverse_included.shape
        == inversion_info.feature_inverses.shape
        == inversion_info.inverted_features_mask.shape
    )

    assert np.all(
        unique_features[inversion_info.inverted_features_mask] >= first_inverted_feature
    )
    assert np.all(
        unique_features[~inversion_info.inverted_features_mask] < first_inverted_feature
    )

    target_feature_files = feature_files[:, target_files]
    for i, row in enumerate(target_feature_files):
        assert np.array_equal(
            row,
            ~target_feature_files[
                unique_features[
                    inversion_info.feature_inverses[unique_features_indices[i]]
                ],
                :,
            ],
        )
