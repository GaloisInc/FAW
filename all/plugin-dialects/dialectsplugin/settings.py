import dataclasses
from typing import Dict, List, Tuple, TypedDict, Literal

from dialectsplugin.filtering import TargetRestrictionMode


@dataclasses.dataclass
class SearchSettings:
    """Settings for the feature search widget"""
    feature_search_expanded: bool = True
    feature_search_regex: str = ''
    feature_search_regex_case_sensitive: bool = False
    sort_order: Literal['ascending', 'descending', 'entropy'] = 'entropy'


@dataclasses.dataclass
class DialectWizardSettings:
    """Settings for the dialect wizard itself"""
    targeted_features_cnf: Dict[int, bool] = (
        dataclasses.field(default_factory=dict)
    )
    """Mapping from feature index to whether feature must be present.

    If empty, target all files.
    """
    find_dialects: bool = False
    # TODO maybe min_entropy or similar would be useful as well?
    min_feature_samples: int = 20
    target_restriction_mode: TargetRestrictionMode = 'homogeneous_outside'
    # restrict_to_target: bool = True
    highlighted_filename: str = ''
    excluded_features: List[int] = ()  # TODO not implemented yet
    """Feature indices to exclude as dialect choices"""
    exclusion_min_attr_risk: float = 0.9
    """Minimum risk attributable to an excluded feature for exclusion"""
    max_slop_files: int = 10
    max_dialects: int = 6
    max_partitions: int = 10
    no_partition_overlap: bool = False

    def __post_init__(self):
        self.targeted_features_cnf = {
            int(feature): included
            for feature, included in self.targeted_features_cnf.items()
        }
        self.excluded_features = [
            int(feature) for feature in self.excluded_features
        ]


class SimilarFeature(TypedDict):
    feature: int
    negated: bool  # TODO not used yet
    attr_risk: float
    """Similarity metric; risk attributable to the other feature"""
    size_target: int
    size_global: int
    highlight: bool


class Dialect(TypedDict):
    hero_feature: int
    similar_features: List[SimilarFeature]
    """Mapping from feature index to similarity of this feature to the hero"""
    negated: bool  # TODO not used yet
    size_target: int
    size_global: int
    highlight: bool
    filenames_outside_target: List[Tuple[str, bool]]


class SlopFile(TypedDict):
    filename: str
    in_dialects: List[int]
    """Indices of dialects containing this file"""
    highlight: bool


class Partition(TypedDict):
    dialects: List[Dialect]
    partition_quality: Dict[str, float]
    """Aggregate partition quality metrics by name"""
    slop_files: List[SlopFile]
