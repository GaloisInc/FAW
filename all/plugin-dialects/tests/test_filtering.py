import collections

import numpy as np
import pytest

from dialectsplugin.filtering import deduplicated_feature_indices, select_valid_heroes


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

    unique_features, unique_features_indices = deduplicated_feature_indices(
        feature_files=feature_files,
        target_files=target_files,
        feature_slop=feature_slop,
    )

    assert set(unique_features) == set(bests.values())

    assert np.unique(feature_files[:, target_files][unique_features, :], axis=0).shape[
        0
    ] == len(unique_features)

    assert np.array_equal(
        feature_files[:, target_files][unique_features, :][unique_features_indices],
        feature_files[:, target_files],
    )


@pytest.mark.parametrize("random_seed", [0, 13546])
@pytest.mark.parametrize("min_feature_samples", [1, 10])
@pytest.mark.parametrize("max_slop_files", [0, 1, 10])
@pytest.mark.parametrize("excluded_features", [[], list(range(10))])
@pytest.mark.parametrize("n_target_files", [100, 70, 2])
@pytest.mark.parametrize("zero_slop", [True, False])
def test_select_valid_heroes(
    random_seed: int,
    min_feature_samples: int,
    max_slop_files: int,
    excluded_features: list[int],
    n_target_files: int,
    zero_slop: bool,
):
    rng = np.random.default_rng(seed=random_seed)
    n_features = 20
    n_files = 100
    target_files = np.arange(n_target_files)
    feature_files = rng.integers(0, 2, (n_features, n_files), dtype=np.bool_)
    feature_slop = rng.integers(
        0, (not zero_slop) * (n_files - n_target_files) + 1, n_features
    )

    valid_heroes = select_valid_heroes(
        feature_files=feature_files,
        target_files=target_files,
        min_feature_samples=min_feature_samples,
        feature_slop=feature_slop,
        excluded_features=np.asarray(excluded_features),
        exclusion_min_attr_risk=0.8,
        max_slop_files=max_slop_files,
    )

    for excluded_feature in excluded_features:
        assert excluded_feature not in valid_heroes

    for feature in valid_heroes:
        distribution = feature_files[feature]
        assert np.sum(distribution[target_files]) >= max(
            min_feature_samples, max_slop_files
        )

        assert feature_slop[feature] <= max_slop_files

    if zero_slop and not excluded_features:
        assert set(valid_heroes) == {
            feature
            for feature, distribution in enumerate(feature_files[:, target_files])
            if (
                n_target_files - max(min_feature_samples, max_slop_files)
                >= np.sum(distribution)
                >= max(min_feature_samples, max_slop_files)
            )
        }
