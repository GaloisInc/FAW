import collections
import dataclasses
import sys
from typing import Any, DefaultDict, Dict, List, Optional, Sequence, Tuple

import jinja2
import numpy as np
import numpy.typing as npt
import typer
import ujson as json

from dialectsplugin.partitions import best_partitions, target_files_from_cnf
from dialectsplugin.quality import quality_metrics
from dialectsplugin.settings import (
    Dialect,
    DialectWizardSettings,
    Partition,
    SearchSettings,
    SimilarFeature,
    SlopFile,
)
from dialectsplugin.similarity import features_attributable_to


def main(workbench_api_url: str, json_arguments: str, output_html: str):
    # Load parameters
    json_args = {
        "search_settings": {},
        "dialect_settings": {},
        **json.loads(json_arguments),
    }
    search_settings = SearchSettings(**json_args["search_settings"])
    dialect_settings = DialectWizardSettings(**json_args["dialect_settings"])

    # build feature matrix
    filename_to_index: Dict[str, int] = {}
    filenames: List[str] = []
    file_indices_by_feature: DefaultDict[str, List[int]] = collections.defaultdict(list)
    for line in sys.stdin:
        line = line.strip()
        if not line:
            continue

        obj: Dict[str, Any] = json.loads(line)
        filename = obj.pop("_id")
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
        highlight_file_index = filename_to_index.get(
            dialect_settings.highlighted_filename
        )

    # feature x file
    # Not bothering with a sparse array yet since they're a royal pain
    feature_files: npt.NDArray[np.bool_] = np.zeros(
        (len(features), len(filename_to_index)), dtype=np.bool_
    )
    for feature, file_indices in file_indices_by_feature.items():
        feature_files[feature_to_index[feature], file_indices] = 1

    # TODO factor some of this into other modules
    if dialect_settings.find_dialects:
        target_files: npt.NDArray[np.int_] = target_files_from_cnf(
            feature_files=feature_files,
            target_features_cnf=dialect_settings.targeted_features_cnf,
        )
        target_size = len(target_files)
        partitions_with_debug_lines = best_partitions(
            feature_files=feature_files,
            target_features_cnf=dialect_settings.targeted_features_cnf,
            min_feature_samples=dialect_settings.min_feature_samples,
            target_restriction_mode=dialect_settings.target_restriction_mode,
            max_slop_files=dialect_settings.max_slop_files,
            excluded_features=np.asarray(
                dialect_settings.excluded_features, dtype=np.int_
            ),
            exclusion_min_attr_risk=dialect_settings.exclusion_min_attr_risk,
            max_dialects=dialect_settings.max_dialects,
            max_partitions=dialect_settings.max_partitions,
            allow_inverted_features=dialect_settings.allow_inverted_features,
            no_partition_overlap=dialect_settings.no_partition_overlap,
            feature_names=features,
        )
        debug_lines = partitions_with_debug_lines.debug_lines

        def build_similar_features(feature: int) -> List[SimilarFeature]:
            similarities_and_features: Sequence[
                Tuple[float, int]
            ] = features_attributable_to(
                feature=feature,
                feature_files=np.concatenate(
                    [feature_files, ~feature_files], axis=0, dtype=np.bool_
                ),
                min_risk=min(0.5, dialect_settings.exclusion_min_attr_risk - 0.05),
                max_attributed_features=10,  # Arbitrary; TODO unhardcode
            )
            similar_features: List[SimilarFeature] = []
            for attr_risk, similar_feature_maybe_inverted in similarities_and_features:
                inverted = similar_feature_maybe_inverted >= feature_files.shape[0]
                similar_feature = (
                    similar_feature_maybe_inverted - feature_files.shape[0]
                    if inverted
                    else similar_feature_maybe_inverted
                )
                size_target = np.sum(target_feature_files[similar_feature])
                size_global = np.sum(feature_files[similar_feature])
                highlight = (
                    feature_files[similar_feature, highlight_file_index]
                    if highlight_file_index is not None
                    else False
                )
                if inverted:
                    size_target = target_feature_files.shape[1] - size_target
                    size_global = feature_files.shape[1] - size_global
                    highlight = highlight_file_index is not None and not highlight
                similar_features.append(
                    SimilarFeature(
                        feature=int(similar_feature),
                        inverted=bool(inverted),
                        attr_risk=float(attr_risk),
                        size_target=int(size_target),
                        size_global=int(size_global),
                        highlight=bool(highlight),
                    )
                )
            return similar_features

        partitions = []
        for partition in partitions_with_debug_lines.value:
            dialect_feature_files = feature_files[partition.hero_features, :]
            # Invert file distributions of inverted heroes in place
            dialect_feature_files[partition.inverted] ^= True
            dialect_target_feature_files = dialect_feature_files[:, target_files]
            target_file_dialect_membership_counts = np.sum(
                dialect_target_feature_files, axis=0
            )
            target_feature_files = feature_files[:, target_files]
            partition_file_indices: List[Sequence[int]] = [
                np.nonzero(row)[0] for row in dialect_target_feature_files
            ]
            partitions.append(
                Partition(
                    dialects=[
                        Dialect(
                            hero_feature=int(hero_feature_index),
                            similar_features=build_similar_features(hero_feature_index),
                            inverted=bool(inverted),
                            size_target=int(
                                np.sum(dialect_target_feature_files[dialect_index, :])
                            ),
                            size_global=int(
                                np.sum(dialect_feature_files[dialect_index, :])
                            ),
                            highlight=(
                                bool(
                                    dialect_feature_files[
                                        dialect_index, highlight_file_index
                                    ]
                                )
                                if highlight_file_index is not None
                                else False
                            ),
                            filenames_outside_target=[
                                (
                                    filenames[file_index],
                                    bool(file_index == highlight_file_index),
                                )
                                for file_index in np.nonzero(
                                    np.delete(
                                        dialect_feature_files[dialect_index, :],
                                        target_files,
                                    )
                                )[0]
                            ],
                        )
                        for dialect_index, (hero_feature_index, inverted) in enumerate(
                            zip(partition.hero_features, partition.inverted)
                        )
                    ],
                    partition_quality={
                        metric_name: float(
                            metric_func(
                                partition_file_indices,
                                target_feature_files,
                            )
                        )
                        for metric_name, metric_func in quality_metrics.items()
                    },
                    slop_files=[
                        SlopFile(
                            filename=filenames[
                                file_index := target_files[target_file_index]
                            ],
                            in_dialects=(
                                dialect_feature_files[:, file_index]
                                .nonzero()[0]
                                .tolist()
                            ),
                            highlight=bool(file_index == highlight_file_index),
                        )
                        for target_file_index in (
                            target_file_dialect_membership_counts != 1
                        ).nonzero()[0]
                    ],
                )
            )
    else:
        partitions = []
        debug_lines = []
        target_size = 0

    with open(output_html, "w") as f, open("./assets/vue.js") as f_vue, open(
        "./assets/main.js"
    ) as f_main, open("./assets/style.css") as f_style, open(
        "./assets/index.html.jinja"
    ) as f_html_template:
        data = {
            "api_url": workbench_api_url,
            "debug_str": "\n".join(debug_lines),
            "feature_text": features,
            "feature_counts": [
                len(file_indices_by_feature[feature_text]) for feature_text in features
            ],
            "num_files": len(filenames),
            "partitions": partitions,
            "target_size": target_size,
            "search_settings": dataclasses.asdict(search_settings),
            "dialect_settings": dataclasses.asdict(dialect_settings),
        }
        f.write(
            jinja2.Template(f_html_template.read()).render(
                vue_source=f_vue.read(),
                css=f_style.read(),
                window_data=json.dumps(data),
                main_source=f_main.read(),
            )
        )


if __name__ == "__main__":
    typer.run(main)
