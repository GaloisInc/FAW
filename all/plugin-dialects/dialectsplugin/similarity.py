from typing import Sequence, Tuple

import numpy as np
import numpy.typing as npt
import scipy.spatial
import scipy.special


def file_distribution_similarity(
    mask_a: npt.NDArray[np.bool_],
    mask_b: npt.NDArray[np.bool_],
    *,
    weights: npt.NDArray[np.float_],
) -> npt.NDArray[np.float_]:
    """Similarity between two file distributions (features).

    Based on (weighted) hamming similarity.

    The two distributions/masks must be broadcastable. The
    last axis is taken to be the file distribution.
    ``weights`` must be parallel to the file distributions (but must be 1-D).

    Exactly matching => 1.0
    Exactly opposite => 0.0
    ``weights`` is normalized to sum to 1, so the above always holds.
    """
    return np.sum((weights / np.sum(weights)) * (mask_a == mask_b), axis=-1)


def file_distribution_attributable_risk(
    antecedent: npt.NDArray[np.bool_],
    consequent: npt.NDArray[np.bool_],
) -> npt.NDArray[np.float_]:
    """Risk attributable to a file distribution.

    The two distributions/masks must be broadcastable. The
    last axis is taken to be the file distribution.

    Entirely attributable => 1.0
    Not at all attributable => 0.0
    Anti-attributable => -1.0
    """
    # TODO Slow calculation; should be improved if we stick with it
    a = np.sum(antecedent & consequent, axis=-1, dtype=np.float_)
    b = np.sum(antecedent & ~consequent, axis=-1, dtype=np.float_)
    c = np.sum(~antecedent & consequent, axis=-1, dtype=np.float_)
    d = np.sum(~antecedent & ~consequent, axis=-1, dtype=np.float_)
    # Assume entirely unrelated if we have 0 in a denominator
    return np.nan_to_num((a / (a + b)) - (c / (c + d)), 0.0, 0.0, 0.0)


def _p_log2_p(p):
    return scipy.special.xlogy(p, p)
    # return p * np.nan_to_num(np.log2(p), nan=0.0)


def bernoulli_entropy_weights(
    *,
    counts: npt.NDArray[np.int_],
    total: int,
) -> npt.NDArray[np.float_]:
    # file probability (over features)
    p = counts / total
    # file entropy
    return -_p_log2_p(p) - _p_log2_p(1 - p)


def similar_features_to(
    feature: int,
    feature_files: npt.NDArray[np.bool_],
    min_similarity: float,
    max_similar_features: int,
) -> Sequence[Tuple[float, int]]:
    file_entropies = bernoulli_entropy_weights(
        counts=np.sum(feature_files, axis=0),
        total=feature_files.shape[0],
    )
    similarities = file_distribution_similarity(
        feature_files[feature, :],
        feature_files,
        weights=file_entropies,
    )
    top_similar_features = np.argpartition(
        similarities,
        -min(max_similar_features, len(similarities)),
    )
    sims_and_feats = [
        (similarities[i], i)
        for i in top_similar_features[-min(max_similar_features, len(similarities)) :]
        if similarities[i] >= min_similarity and i != feature
    ]
    sims_and_feats.sort(reverse=True)
    return sims_and_feats


def features_attributable_to(
    feature: int,
    feature_files: npt.NDArray[np.bool_],
    min_risk: float,
    max_attributed_features: int,
) -> Sequence[Tuple[float, int]]:
    # # TODO Can/should we weight these?
    # file_entropies = bernoulli_entropy_weights(
    #     file_counts=np.sum(feature_files, axis=0),
    #     total_features=feature_files.shape[0],
    # )
    attr_risks = file_distribution_attributable_risk(
        feature_files[feature, :],
        feature_files,
    )
    top_similar_features = np.argpartition(
        attr_risks,
        -min(max_attributed_features, len(attr_risks)),
    )
    sims_and_feats = [
        (attr_risks[i], i)
        for i in top_similar_features[-min(max_attributed_features, len(attr_risks)) :]
        if attr_risks[i] >= min_risk and i != feature
    ]
    sims_and_feats.sort(reverse=True)
    return sims_and_feats


def pairwise_attributable_risk(
    antecedent_feature_files: npt.NDArray[np.bool_],
    consequent_feature_files: npt.NDArray[np.bool_],
) -> npt.NDArray[np.float_]:
    """
    Args:
        antecedent_feature_files: m*n matrix of m features n files
        consequent_feature_files: k*n matrix of k features over n files
    Returns:
        m*k matrix of risk attributable to each antecedent feature by each
        consequent feature
    """
    # TODO can do this faster (can eliminate at least one matrix op using a sum)
    antecedent_feature_files_complement = ~antecedent_feature_files
    consequent_feature_files_complement = ~consequent_feature_files
    # Use np.matmul instead of @ notation to convert to float matrices
    # (otherwise we end up with int matrices)
    a = np.matmul(antecedent_feature_files, consequent_feature_files.T, dtype=np.float_)
    b = np.matmul(
        antecedent_feature_files, consequent_feature_files_complement.T, dtype=np.float_
    )
    c = np.matmul(
        antecedent_feature_files_complement, consequent_feature_files.T, dtype=np.float_
    )
    d = np.matmul(
        antecedent_feature_files_complement,
        consequent_feature_files_complement.T,
        dtype=np.float_,
    )
    # Assume entirely unrelated if we have 0 in a denominator
    return np.nan_to_num((a / (a + b)) - (c / (c + d)), 0.0, 0.0, 0.0)
