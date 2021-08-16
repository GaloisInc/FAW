"""Shared utility methods for FAW analysis sets.
"""

import collections
import pymongo
import sys

PipelineParserSpec = collections.namedtuple('PipelineParserSpec', ['pipe',
        'parser', 'aset'])


def aset_pipeline_parsers(app_config, aset):
    """Given an analysis set document `aset`, returns a mapping of
    `{pipeline aset: [(pipeline_name, parser_name)]}` which indicates the
    pipelines from which the given analysis set requires parser information.

    This list excludes those parsers which have been disabled in config.json5
    """
    pipes = collections.defaultdict(set)
    for pr in aset['definition']['rules']:
        parts = deconstruct_pipeline_parser_name(pr['parser'])
        if parts is None:
            continue

        # Ensure not disabled!
        pipe_cfg = app_config['pipelines'][parts.pipe]
        if pipe_cfg.get('disabled'):
            continue
        if (pipe_cfg['parsers']
                .get(parts.parser, {'disabled': True})
                .get('disabled')):
            continue

        # This one needs to run
        pipes[parts.aset].add((parts.pipe, parts.parser))
    return pipes


def deconstruct_pipeline_parser_name(parser_name):
    """Deconstruct a compound parser name into
    `(pipe_name, parser_name, aset_name)`. Returns `None` if `parser_name` is not
    a compound name.
    """
    parts = parser_name.split('-~-')
    assert len(parts) in (1, 3), parts
    if len(parts) == 3:
        return PipelineParserSpec(parts[0], parts[1], parts[2])
    return None


def lookup_pipeline_parser_name(aset_name, pipe_name, parser_name):
    """Find the flattened name of a pipeline parser running under an analysis
    set.
    """
    return f'{pipe_name}-~-{parser_name}-~-{aset_name}'


def lookup_pipeline_parser_versions(app_config, pipe_name, parser_name,
        aset_pipe_done):
    """Looks up `(version_parser, version_parser_parser)` for a pipeline parser.
    This is a dynamic version depending on the pipeline's done time.

    Returns as a list instead of a tuple because that's how it gets stored in
    mongodb, and otherwise equivalence checking might fail!
    """
    pipe = app_config['pipelines'][pipe_name]
    parser = pipe['parsers'][parser_name]

    return [
            f"{parser['version']}-~-{aset_pipe_done}",
            parser['parse']['version']]


def lookup_all_parsers(db, app_config, exclude_unfinished=False):
    """Active parser set depends on analysis sets. Look up an augmented version
    containing all parsers.
    """
    assert isinstance(db, pymongo.database.Database), db
    parsers = app_config['parsers'].copy()
    for adoc in db['as_metadata'].find():
        aset_pipelines = adoc.get('pipelines', {})
        for k, v in aset_pipelines.items():
            if exclude_unfinished and not v.get('done'):
                # Not yet ready to parse with this pipeline's parsers
                continue

            if k not in app_config['pipelines']:
                # Not found?
                continue

            for pk, pv in app_config['pipelines'][k]['parsers'].items():
                ppv = pv.copy()
                ppv['aset'] = adoc['_id']
                ppv['pipeline'] = k
                parsers[lookup_pipeline_parser_name(adoc['_id'], k, pk)] = ppv
    return parsers

