import collections
import dataclasses
from typing import Any, DefaultDict, Dict, List, Optional, Sequence, Set, Tuple, TypedDict, Literal
import ujson as json
import numpy as np
import numpy.typing as npt
import re
import scipy.sparse
import scipy.stats
import sys
import typer
import jinja2

import partitioning


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
    target_restriction_mode: partitioning.TargetRestrictionMode = 'homogeneous_outside'
    # restrict_to_target: bool = True
    highlighted_filename: str = ''
    excluded_features: List[int] = ()  # TODO not implemented yet
    """Feature indices to exclude as dialect choices"""
    exclusion_min_attr_risk: float = 0.9
    """Minimum risk attributable to an excluded feature for exclusion"""
    max_slop_files: int = 10

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
    filenames_outside_target: List[str]


class SlopFile(TypedDict):
    filename: str
    in_dialects: List[int]
    """Indices of dialects containing this file"""


class Partition(TypedDict):
    dialects: List[Dialect]
    partition_quality: Dict[str, float]
    """Aggregate partition quality metrics by name"""
    slop_files: List[SlopFile]


def main(workbench_api_url: str, json_arguments: str, output_html: str):

    # Load parameters
    json_args = {'search_settings': {}, 'dialect_settings': {}, **json.loads(json_arguments)}
    search_settings = SearchSettings(**json_args['search_settings'])
    dialect_settings = DialectWizardSettings(**json_args['dialect_settings'])

    # build feature matrix
    filename_to_index: Dict[str, int] = {}
    filenames: List[str] = []
    file_indices_by_feature: DefaultDict[str, List[int]] = collections.defaultdict(list)
    for line in sys.stdin:
        line = line.strip()
        if not line:
            continue

        obj: Dict[str, Any] = json.loads(line)
        filename = obj.pop('_id')
        file_index = len(filename_to_index)
        filenames.append(filename)
        filename_to_index[filename] = file_index

        for feature, feature_value in obj.items():
            file_indices_by_feature[feature].append(file_index)

    features = list(file_indices_by_feature.keys())
    feature_to_index: Dict[str, int] = {
        feature: i for i, feature in enumerate(features)
    }

    highlight_file_index: Optional[int] = None
    if dialect_settings.highlighted_filename:
        highlight_file_index = filename_to_index.get(dialect_settings.highlighted_filename)

    # feature x file
    # Not bothering with a sparse array yet since they're a royal pain
    feature_files: npt.NDArray[np.bool_] = np.zeros(
        (len(features), len(filename_to_index)), dtype=np.bool_
    )
    for feature, file_indices in file_indices_by_feature.items():
        feature_files[feature_to_index[feature], file_indices] = 1

    # Now get partition(s) using API in partitioning package
    # TODO move this logic into the package too
    if dialect_settings.find_dialects:
        target_files: npt.NDArray[np.int_] = partitioning.target_files_from_cnf(
            feature_files,
            dialect_settings.targeted_features_cnf,
        )
        target_size = len(target_files)
        partition_hero_feature_indices, debug_lines = partitioning.best_partitions(
            feature_files,
            target_features_cnf=dialect_settings.targeted_features_cnf,
            min_feature_samples=dialect_settings.min_feature_samples,
            target_restriction_mode=dialect_settings.target_restriction_mode,
            max_slop_files=dialect_settings.max_slop_files,
            excluded_features=dialect_settings.excluded_features,
            exclusion_min_attr_risk=dialect_settings.exclusion_min_attr_risk,
            feature_names=features,
        )

        def build_similar_features(feature: int) -> List[SimilarFeature]:
            similarities_and_features: Sequence[Tuple[float, int]] = partitioning.features_attributable_to(
                feature=feature,
                feature_files=feature_files,
                min_risk=min(0.25, dialect_settings.exclusion_min_attr_risk - 0.05),
                max_attributed_features=10,  # Arbitrary; TODO unhardcode
            )
            similar_features: List[SimilarFeature] = []
            for attr_risk, similar_feature in similarities_and_features:
                similar_features.append(SimilarFeature(
                    feature=int(similar_feature),
                    negated=False,
                    attr_risk=float(attr_risk),
                    size_target=float(np.sum(target_feature_files[similar_feature])),
                    size_global=float(np.sum(feature_files[similar_feature])),
                    highlight=(
                        bool(feature_files[similar_feature, highlight_file_index])
                        if highlight_file_index is not None else False
                    ),
                ))
            return similar_features

        partitions = []
        for dialect_hero_feature_indices in partition_hero_feature_indices:
            dialect_feature_files = feature_files[dialect_hero_feature_indices, :]
            target_file_dialect_membership_counts = np.sum(
                dialect_feature_files[:, target_files], axis=0
            )
            target_feature_files = feature_files[:, target_files]
            partition_file_indices: List[Sequence[int]] = [
                np.nonzero(target_feature_files[hero_feature_index, :])[0]
                for hero_feature_index in dialect_hero_feature_indices
            ]
            partitions.append(
                Partition(
                    dialects=[
                        Dialect(
                            hero_feature=int(hero_feature_index),
                            similar_features=build_similar_features(hero_feature_index),
                            negated=False,
                            size_target=float(np.sum(target_feature_files[hero_feature_index, :])),
                            size_global=float(np.sum(feature_files[hero_feature_index, :])),
                            highlight=(
                                bool(feature_files[hero_feature_index, highlight_file_index])
                                if highlight_file_index is not None else False
                            ),
                            filenames_outside_target=[
                                filenames[file_index]
                                for file_index in np.nonzero(
                                    np.delete(feature_files[hero_feature_index, :], target_files)
                                )[0]
                            ],
                        )
                        for hero_feature_index in dialect_hero_feature_indices
                    ],
                    partition_quality={
                        metric_name: float(metric_func(
                            partition_file_indices,
                            target_feature_files,
                        ))
                        for metric_name, metric_func in partitioning.quality_metrics.items()
                    },
                    slop_files=[
                        SlopFile(
                            filename=filenames[file_index := target_files[target_file_index]],
                            in_dialects=dialect_feature_files[:, file_index].nonzero()[0].tolist(),
                        )
                        for target_file_index in (target_file_dialect_membership_counts != 1).nonzero()[0]
                    ],
                )
            )
    else:
        partitions = []
        debug_lines = []
        target_size = 0

    with open(output_html, 'w') as f, \
            open('./assets/vue.js') as f_vue, \
            open('./assets/main.js') as f_main, \
            open('./assets/style.css') as f_style, \
            open('./assets/index.html.jinja') as f_html_template:
        data = {
            'api_url': workbench_api_url,
            'debug_str': '\n'.join(debug_lines),
            'feature_text': features,
            'feature_counts': [
                len(file_indices_by_feature[feature_text]) for feature_text in features
            ],
            'num_files': len(filenames),
            'partitions': partitions,
            'target_size': target_size,
            'search_settings': dataclasses.asdict(search_settings),
            'dialect_settings': dataclasses.asdict(dialect_settings),
        }
        f.write(jinja2.Template(f_html_template.read()).render(
            vue_source=f_vue.read(),
            css=f_style.read(),
            window_data=json.dumps(data),
            main_source=f_main.read(),
        ))


if __name__ == '__main__':
    typer.run(main)

