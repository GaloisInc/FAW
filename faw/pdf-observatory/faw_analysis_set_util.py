"""Shared utility methods for FAW analysis sets.
"""

import collections
import pymongo
import sys

PipelineParserSpec = collections.namedtuple('PipelineParserSpec', ['pipe',
        'parser', 'aset'])


def aset_parser_versions_calculate(app_config, db, aset):
    """Given an application config, an analysis set document `aset`, and a
    pymongo `Database` object, return version information for all parsers:

        `(versions_id, versions_data)`
        versions_id = {parser: Union[None, [tool version, parser versino]]}
        versions_data = same

    These versions specify when set membership needs to be recomputed and when
    cached data must be recomputed.
    """

    # ID versions are those which show up in `features` on the aset definition.
    fts = aset['definition'].get('features', [])
    id_parsers = set([f['parser'] for f in fts])
    versions_id = _aset_parser_versions_calculate_list(db, app_config, id_parsers)

    # Data versions are those which show up in `rules`
    id_data = set([f['parser'] for f in aset['definition']['rules']])
    versions_data = _aset_parser_versions_calculate_list(db, app_config, id_data)

    return versions_id, versions_data


def aset_parser_versions_calculate_idle(app_config):
    """Computes the parser versions for the idle parser.

    Always runs immediately, never hits DB.
    """
    id_parsers = set([k for k, v in app_config['parsers'].items()
            if not v.get('disabled')])
    versions_id = {}
    versions_data = _aset_parser_versions_calculate_list(None, app_config, id_parsers)
    return versions_id, versions_data


def _aset_parser_versions_calculate_list(db, app_config, parser_list):
    """Given some list of parsers `parser_list`, use `db` and `app_config` to
    determine precise versions of each.

    For any pipeline which is not yet finished processing, or for any parser
    that is disabled or no longer configured, `None` will be used instead of
    versions for that parser.

    Returns:
        {parser_name: Union[None, [parser_tool_version, parser_parser_version]]}

    Where:
        parser_tool_version = {processor name or '': str}
        parser_parser_version = {processor name or '': str}

    The `processor name` field helps with e.g. data transformation pipelines or
    additional, global parser-parsers that run on all program output. E.g.,
    `parser_parsers_shared`.
    """

    aset_cache = {}

    parser_parsers = {}
    for ppk, ppv in app_config['parser_parsers_shared'].items():
        if ppv['disabled']:
            parser_parsers[ppk] = None
            continue
        ppv_v = ppv['parse']['version']
        parser_parsers[ppk] = ppv_v

    r = {}
    for p_name in parser_list:
        pipe = deconstruct_pipeline_parser_name(p_name)
        p_cfg = None
        tool_version = None
        if pipe is None:
            # Standard parser
            p_cfg = app_config['parsers'].get(p_name, {'disabled': True})
            if p_cfg['disabled']:
                r[p_name] = None
                continue

            tool_version = p_cfg['version']
        else:
            # Pipeline
            pipe_cfg = app_config['pipelines'].get(pipe.pipe, {'disabled': True})
            if pipe_cfg['disabled']:
                r[p_name] = None
                continue

            p_cfg = pipe_cfg['parsers'][pipe.parser]
            if p_cfg['disabled']:
                r[p_name] = None
                continue

            # Fetch document; see if it has completed execution
            if pipe.aset not in aset_cache:
                aset_cache[pipe.aset] = db['as_metadata'].find_one({
                        '_id': pipe.aset})
            paset = aset_cache[pipe.aset] or {}
            p_done = paset.get('pipelines', {}).get(pipe.pipe, {}).get('done')

            if p_done is None:
                r[p_name] = None
                continue

            tool_version = f'{p_cfg["version"]}-~-{p_done}'

        tool = {'': tool_version}
        parse = {'': p_cfg['parse']['version'], **parser_parsers}
        r[p_name] = [tool, parse]
    return r


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


def lookup_all_parsers(db, app_config, exclude_unfinished=False):
    """Active parser set depends on analysis sets. Look up an augmented version
    containing all parsers.

    This method is used more by the UI -- `calculate_parser_versions` does the more
    in-depth analysis used by the backend.
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

