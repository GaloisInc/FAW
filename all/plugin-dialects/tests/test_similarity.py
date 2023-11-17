import numpy as np

from dialectsplugin.similarity import file_distribution_attributable_risk

def test_file_distribution_attributable_risk():
    dist1 = np.array([1,1,1,0,0,0], dtype=bool)

    assert file_distribution_attributable_risk(
        dist1, dist1
    ) == 1.0
    assert file_distribution_attributable_risk(
        dist1, ~dist1
    ) == -1.0
    assert file_distribution_attributable_risk(
        dist1, np.ones_like(dist1)
    ) == 0.0
    assert np.isclose(file_distribution_attributable_risk(
        dist1, np.array([1,1,1,1,0,0], dtype=bool)
    ), 2/3)

