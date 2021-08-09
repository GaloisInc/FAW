"""Shared utility methods for FAW analysis sets.
"""

import pymongo
import sys

def lookup_pipeline_parser_name(aset_name, pipe_name, parser_name):
    """Find the flattened name of a pipeline parser running under an analysis
    set.
    """
    return f'{pipe_name}--{parser_name}--{aset_name}'


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

