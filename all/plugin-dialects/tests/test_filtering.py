import collections

import numpy as np

from dialectsplugin.filtering import deduplicated_feature_indices


def test_deduplicated_feature_indices():
    rng = np.random.default_rng(seed=0)
    # More features than files, so we're pigeonholed into seeing duplicates
    n_features = 20
    n_files = 7
    target_files = np.arange(4)
    feature_files = rng.integers(0, 2, (n_features, n_files), dtype=np.bool_)
    feature_slop = rng.random(n_features)

    dupe_groups = collections.defaultdict(set)
    for i, row in enumerate(feature_files):
        dupe_groups[tuple(row[target_files])].add(i)
    bests = {
        distribution: min(dupes, key=feature_slop.__getitem__)
        for distribution, dupes in dupe_groups.items()
    }
    assert len(dupe_groups) < n_features, "No duplicate rows--change seed or dimensions"

    unique_indices, inverse = deduplicated_feature_indices(
        feature_files=feature_files,
        target_files=target_files,
        feature_slop=feature_slop,
    )

    assert set(unique_indices) == set(bests.values())

    assert (
        np.unique(feature_files[:, target_files][unique_indices, :], axis=0).shape[0]
        == len(unique_indices)
    )

    assert np.array_equal(
        feature_files[:, target_files][unique_indices, :][inverse],
        feature_files[:, target_files]
    )
