"""ParserDependencyGraph and other utilities for dealing with parser
artifacts and interdependency.
"""
import collections
import graphlib
from typing import AbstractSet, Collection, DefaultDict, Dict, Any, Mapping, Sequence, Set

import faw_substitutions


class ParserDependencyGraph:
    """Given parser configs, tracks parsers' production/consumption of artifacts.

    Whenever used, this class should be given parser configs associated
    with _all_ asets.

    Disabled parsers are not included.

    Note that only some methods raise an error on cyclic dependency.
    """

    parsers_producing_artifacts_of_type: DefaultDict[str, Set[str]]
    """Artifact type -> immediate upstream parsers"""
    parsers_consuming_artifacts_of_type: DefaultDict[str, Set[str]]
    """Artifact type -> immediate downstream parsers"""
    artifact_types_by_parser: Dict[str, faw_substitutions.ArtifactTypes]
    """Parser name -> immediate artifact dependencies"""

    def __init__(self, parser_configs: Mapping[str, Mapping[str, Any]]) -> None:
        self.parsers_producing_artifacts_of_type = collections.defaultdict(set)
        self.parsers_consuming_artifacts_of_type = collections.defaultdict(set)
        self.artifact_types_by_parser = {}

        for k, cfg in parser_configs.items():
            if cfg.get('disabled', False):
                continue
            artifact_types = faw_substitutions.artifact_types(cfg['exec'])
            self.artifact_types_by_parser[k] = artifact_types
            for artifact_type in artifact_types.output_artifact_types:
                self.parsers_producing_artifacts_of_type[artifact_type].add(k)
            for artifact_type in artifact_types.input_artifact_types:
                self.parsers_consuming_artifacts_of_type[artifact_type].add(k)

    def parsers_upstream_from_artifact_types(self, artifact_types: AbstractSet[str]) -> Sequence[str]:
        """Return all parsers needed to generate the specified artifacts,
        sorted by dependency.

        Raise ``graphlib.CycleError`` if not possible.
        """
        parsers: Set[str] = set()
        self._add_upstream_parsers_from_artifact_types_to(
            artifact_types=artifact_types, parsers=parsers
        )
        return self.sort_parsers(parsers)

    def parsers_upstream_from_parsers(self, parsers: Collection[str]) -> Set[str]:
        """Return all parsers that are necessary for ``parsers`` to run.
        Some of ``parsers`` may be included in the returned set.
        """
        upstream_parsers: Set[str] = set()
        for parser in parsers:
            self._add_upstream_parsers_from_artifact_types_to(
                artifact_types=self.artifact_types_by_parser[parser].input_artifact_types,
                parsers=upstream_parsers,
            )
        return upstream_parsers

    def _add_upstream_parsers_from_artifact_types_to(
        self,
        *,
        artifact_types: AbstractSet[str],
        parsers: Set[str],
    ) -> None:
        """Add parsers producing the given artifact types to ``parsers``."""
        for artifact_type in artifact_types:
            for upstream_parser in self.parsers_producing_artifacts_of_type[artifact_type]:
                if upstream_parser not in parsers:
                    parsers.add(upstream_parser)
                    self._add_upstream_parsers_from_artifact_types_to(
                        artifact_types=self.artifact_types_by_parser[upstream_parser].input_artifact_types,
                        parsers=parsers,
                    )

    def parsers_downstream_from_parsers(self, parsers: Collection[str]) -> Set[str]:
        """Return all parsers that can be affected by running ``parsers``.
        Some of ``parsers`` may be included in the returned set.
        """
        downstream_parsers: Set[str] = set()
        for parser in parsers:
            self.add_parsers_downstream_from_parser_to(
                parser,
                downstream_parsers,
            )
        return downstream_parsers

    def add_parsers_downstream_from_parser_to(self, parser_name: str, parsers: Set[str]) -> None:
        """Add parsers downstream from the given parser to ``parsers``."""
        artifact_types = self.artifact_types_by_parser[parser_name]
        for artifact_type in artifact_types.output_artifact_types:
            for downstream_parser in self.parsers_producing_artifacts_of_type[artifact_type]:
                if downstream_parser not in parsers:
                    parsers.add(parser_name)
                    self.add_parsers_downstream_from_parser_to(downstream_parser, parsers)

    def sort_parsers(self, parsers: Collection[str]) -> Sequence[str]:
        """Return parser names sorted by dependency.

        Raise ``graphlib.CycleError`` if not possible.
        """
        return graphlib.TopologicalSorter({
            parser: set().union(*(
                self.parsers_producing_artifacts_of_type[artifact_type]
                for artifact_type in self.artifact_types_by_parser[parser].input_artifact_types
            ))
            for parser in parsers
        }).static_order()
