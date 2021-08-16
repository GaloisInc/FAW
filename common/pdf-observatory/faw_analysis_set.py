"""NOTE - this module is a bit dangerous! It mixes async and dask functionality,
leaving it unclear when global variables are valid.
"""

import faw_analysis_set_parse
import faw_analysis_set_util
import faw_internal_util
import faw_pipelines_util

import asyncio
import collections
import dask.distributed
import enum
import pymongo
import random
import re
import sys
import time
import traceback

_app_config = None
_app_config_version = 0

class AsState(enum.Enum):
    UP_TO_DATE = ''
    STALE = 'stale'  # Like up-to-date, but parsers ran after this. UI only
    REBUILD = 'rebuild'
    REBUILDING = 'rebuilding'
    DELETE = 'delete'

def config_update(app_config):
    """Update app config; will trigger
    """
    global _app_config, _app_config_version
    _app_config = app_config
    _app_config_version += 1


async def main_loop(app_mongodb_conn, app_config, get_api_info):
    # Global connection used for web server API
    global _app_mongodb_conn
    _app_mongodb_conn = app_mongodb_conn
    config_update(app_config)
    # Ensure we don't erroneously use the non-underscore version
    del app_config

    # Launch other management tasks both as asynchronous threads and via dask
    api_info = get_api_info()

    client = None
    client_tasks = {}
    while True:
        try:
            if client is None:
                # This can fail sometimes; let it raise an error and try again
                client = await dask.distributed.Client('localhost:8786',
                        asynchronous=True)

            last_queueStop = await _as_last_queueStop()

            # Let dask forget about defunct tasks; they'll cancel themselves
            new_tasks = {}

            # Ensure we have a task that's kicking off parses
            parse_task = client_tasks.get('as_parse')
            if parse_task is not None and parse_task[1] != _app_config_version:
                parse_task = None
            if parse_task is not None:
                try:
                    await parse_task[0].result(timeout=0.5)
                except dask.distributed.TimeoutError:
                    pass
                except:
                    traceback.print_exc()
                    parse_task = None
            if parse_task is None:
                # Spin up
                new_tasks['as_parse'] = (
                        client.submit(
                            faw_analysis_set_parse.as_parse_main,
                            _app_config, api_info,
                            pure=False),
                        _app_config_version)
            else:
                new_tasks['as_parse'] = parse_task

            promises = []
            async for aset in _app_mongodb_conn['as_metadata'].find():
                promises.append(_as_manage(aset, api_info, last_queueStop,
                        client, client_tasks))
                promises.append(_as_manage_pipelines(aset, api_info, client,
                        client_tasks))
            if promises:
                task_objs = await asyncio.gather(*promises)
                for t in task_objs:
                    if t is None:
                        continue
                    new_tasks.update(t)
            client_tasks = new_tasks
        except:
            traceback.print_exc()

        await asyncio.sleep(1.)


async def as_delete(id):
    await _app_mongodb_conn['as_metadata'].update_one({'_id': id}, {'$set': {
            'status': AsState.DELETE.value}})


async def as_list():
    """List non-deleted fields.
    """
    last_queueStop = await _as_last_queueStop()

    r = []
    async for a in _app_mongodb_conn['as_metadata'].find(
            {'status': {'$ne': AsState.DELETE.value}}).sort('_id'):
        name = a['_id']
        colname = f'as_c_{name}'
        col = _app_mongodb_conn[colname]
        stats = await _app_mongodb_conn.command('collstats', colname)

        adoc = dict(id=a['_id'], size_docs=await col.estimated_document_count(),
                size_disk=stats['storageSize'], status=a['status'],
                definition=a['definition'], pipelines=a.get('pipelines', {}))

        if adoc['status'] == AsState.UP_TO_DATE.value:
            if a['last_queueStop'] != last_queueStop:
                adoc['status'] = AsState.STALE.value

        r.append(adoc)
    return r


async def as_pipeline_start(id, pipeline):
    await _app_mongodb_conn['as_metadata'].update_one({'_id': id}, {'$set': {
            'pipelines.' + pipeline: {'done': None, 'timestamp': time.time()},
    }})


async def as_pipeline_delete(id, pipeline):
    await _app_mongodb_conn['as_metadata'].update_one({'_id': id}, {'$unset': {
            'pipelines.' + pipeline: True,
    }})


async def as_update(id, definition):
    await _app_mongodb_conn['as_metadata'].update_one({'_id': id}, {'$set': {
            '_id': id,
            'definition': definition,
            'status': AsState.REBUILD.value
    }}, upsert=True)


########################################################
# Management code

'''
Internally, analysis sets are two collections plus a metadata document.

Metadata document: _id, definition, status. Maybe other fields:
    last_queueStop: Last queueStop. Used when index is finished building to
            set status to either UP_TO_DATE or STALE

Index collection: _id of files included in set.

Stats collection: old statsbyfile, but new schema.
'''

async def _as_last_queueStop():
    last_queueStop_doc = (await _app_mongodb_conn['observatory'].find(
            {'queueStop': {'$ne': None}}).sort('queueStop', -1)
            .limit(1).to_list(1))
    if last_queueStop_doc:
        last_queueStop = last_queueStop_doc[0]['queueStop']
    else:
        last_queueStop = None
    return last_queueStop


async def _as_manage(aset, api_info, last_queueStop, client, client_tasks):
    name = aset['_id']
    mongo_info = api_info['mongo']

    old_task_info = client_tasks.get(name)
    missing = {}
    old_task_result = missing
    if old_task_info is not None and old_task_info[1] != _app_config_version:
        old_task_info = None
    elif old_task_info is not None:
        try:
            old_task_result = await old_task_info[2].result(timeout=0.5)
        except dask.distributed.TimeoutError:
            pass
        except:
            # Another exception -- still pass, but unset the old task
            traceback.print_exc()
            old_task_info = None

    if aset['status'] == AsState.UP_TO_DATE.value:
        # See if stale -- standard parsing queues
        mark_stale = False

        if aset['last_queueStop'] != last_queueStop:
            # Rebuild, but only if processing has halted
            d = await _app_mongodb_conn['observatory'].find_one({
                    'queueStop': None})
            if d is None:
                mark_stale = 'last_queueStop'

        # See if stale -- did a pipeline we depend on finish recently?
        pipes = faw_analysis_set_util.aset_pipeline_parsers(_app_config, aset)
        for pipe_aset_name, pipe_pipes in pipes.items():
            d = await _app_mongodb_conn['as_metadata'].find_one({
                    '_id': pipe_aset_name})
            if d is None:
                # Deleted the reliant analysis set
                continue

            for p, p_parser in pipe_pipes:
                p_done = d.get('pipelines', {}).get(p, {}).get('done')
                p_name = faw_analysis_set_util.lookup_pipeline_parser_name(
                        pipe_aset_name, p, p_parser)
                if p_done is not None:
                    versions = faw_analysis_set_util.lookup_pipeline_parser_versions(
                            _app_config, p, p_parser, p_done)
                    if versions != aset.get('parser_versions', {}).get(p_name):
                        mark_stale = p

        if mark_stale:
            # Everything is done. Set to rebuild
            print(f'analysis_set: Rebuilding {name} due to staleness; {mark_stale}')
            await _app_mongodb_conn['as_metadata'].update_one({'_id': name},
                    {'$set': {'status': AsState.REBUILD.value}})
        return

    if aset['status'] == AsState.DELETE.value:
        # Internal delete -- drop all associated collections
        if old_task_info is not None:
            # Loosely wait for task to exit
            return

        # Give task time to exit
        await asyncio.sleep(5)
        # Purge collections
        await _app_mongodb_conn['as_c_' + name].drop()
        await _app_mongodb_conn['as_i_' + name].drop()
        await _app_mongodb_conn['as_itmp_as_i_' + name].drop()
        # Purge pipelines
        aset_api_info = api_info.copy()
        aset_api_info['aset'] = aset['_id']
        def run():
            api = faw_pipelines_util.Api(aset_api_info, _app_mongodb_conn.delegate)
            api.destructive__purge_aset()
        loop = asyncio.get_running_loop()
        await loop.run_in_executor(None, run)
        # Finally, delete any record of the analysis set
        await _app_mongodb_conn['as_metadata'].delete_one({'_id': name})
        return

    if aset['status'] == AsState.REBUILD.value:
        # Ensure the user doesn't see documents available -- that would be
        # confusing.
        await _app_mongodb_conn['as_c_' + name].drop()

        if old_task_info is not None and old_task_info[0] == AsState.REBUILD:
            if old_task_result is not missing:
                await _app_mongodb_conn['as_metadata'].update_one(
                        {'_id': name, 'status': AsState.REBUILD.value},
                        {'$set': {'status': AsState.REBUILDING.value,
                            # Note the time of last included change.
                            'last_queueStop': old_task_info[3]}})
                # Let this fall through
                aset['status'] = AsState.REBUILDING.value
            else:
                # Keep old task alive
                return {name: old_task_info}
        else:
            # Need to rebuild indices
            future = client.submit(_as_populate_ids, name, mongo_info, _app_config,
                    pure=False)
            return {name: (AsState.REBUILD, _app_config_version, future, last_queueStop)}

    if aset['status'] == AsState.REBUILDING.value:
        if old_task_info is not None and old_task_info[0] == AsState.REBUILDING:
            if old_task_result is not missing:
                # Finished with this step
                await _app_mongodb_conn['as_metadata'].update_one({'_id': name},
                        {'$set': {'status': AsState.UP_TO_DATE.value}})
                return
            else:
                # Keep old task alive
                return {name: old_task_info}
        future = client.submit(_as_populate, name, mongo_info, _app_config,
                pure=False)
        return {name: (AsState.REBUILDING, _app_config_version, future)}


async def _as_manage_pipelines(aset, api_info, client, client_tasks):
    """Spawn / ensure pipeline manager for this analysis set. This only ensures
    that pipelines running under this analysis set get ran; it has nothing to
    do with the parsers.
    """
    name = aset['_id'] + '!__pipelines'
    old_task_info = client_tasks.get(name)
    missing = {}
    old_task_result = missing
    if old_task_info is not None:
        try:
            old_task_result = await old_task_info[0].result(timeout=0.5)
        except dask.distributed.TimeoutError:
            pass
        except:
            traceback.print_exc()
            old_task_info = None

    if old_task_info is not None and old_task_info[1] != _app_config_version:
        # Version change -- invalidate old task and start a new one
        # Do this over a "return" instead of passing through s.t. there's a
        # small delay which reduces (but does NOT eliminate) race conditions
        return

    if old_task_info is not None and old_task_result is missing:
        return {name: old_task_info}

    # New management needed
    # Import here to break cyclical dependency
    import faw_pipelines
    future = client.submit(faw_pipelines.pipeline_admin, _app_config['pipelines'],
            api_info, aset['_id'], pure=False)
    return {name: [future, _app_config_version]}


def _as_populate(as_name, mongo_info, app_config):
    """After IDs matching the analysis set have been specified, information for
    each of those IDs must be gathered.

    Must be re-entrant.
    """
    # Convergent
    db = faw_internal_util.mongo_api_info_to_db_conn(mongo_info)
    col_as_metadata = db['as_metadata']
    as_doc = col_as_metadata.find_one({'_id': as_name})

    col_ids = db['as_i_' + as_name]
    col_parse = db[faw_analysis_set_parse.COL_NAME]
    col_dst = db['as_c_' + as_name]

    # Stage 1 -- run pipeline parsers on files as needed
    _as_populate_pipelines(app_config, as_doc, col_as_metadata, col_ids,
            col_parse)

    if faw_internal_util.dask_check_if_cancelled():
        return

    # Stage 2 -- collect parser information
    _as_populate_gather(as_doc, col_ids, col_dst)


def _as_populate_pipelines(app_config, as_doc, col_as_metadata, col_ids,
        col_parse):
    """Scan through documents which require additional parsing. Use the version
    of all pipelines that was specified when the ids were populated so that we
    have a nice, uniform parse across all files.

    Must be re-entrant.
    """
    batch_size = 128

    parser_versions_done = as_doc.get('parser_versions_done', {})
    if as_doc['parser_versions'] != parser_versions_done:
        # Stage 1
        # Some versions do not match. Loop through all documents, setting
        # `done_parse = 'maybe'` to indicate this. Only do this with documents
        # where `done_parse == False` for re-entrant optimization.

        # To minimize file transfers, add parsers required to each
        # document, and let another process run all parsers on a file in a single
        # batch.

        new_parsers = {}
        for k, v in as_doc['parser_versions'].items():
            if parser_versions_done.get(k) != v:
                new_parsers[k] = v

        while True:
            if faw_internal_util.dask_check_if_cancelled():
                return
            ids = [d['_id'] for d in col_ids.find({'done_parse': False}, {})
                    .limit(batch_size)]
            if not ids:
                break

            updates = [
                    pymongo.UpdateOne({'_id': i},
                        {'$set': {'parsers.' + k: v for k, v in new_parsers.items()}},
                        upsert=True)
                    for i in ids]
            col_parse.bulk_write(updates)

            # Success - onward!
            col_ids.update_many({'_id': {'$in': ids}}, {'$set': {
                    'done_parse': 'maybe'}})

    # Stage 2 -- wait for all documents to complete all needed parsers
    while True:
        if faw_internal_util.dask_check_if_cancelled():
            return
        ids = [d['_id'] for d in col_ids.find({'done_parse': 'maybe'}, {})
                .limit(batch_size)]
        if not ids:
            break

        # These are done when a `col_parse` entry either doesn't exist or is
        # missing all of our parser_versions entries.
        matching = list(col_parse.find({'_id': {'$in': ids}}))

        finished = set(ids)
        for m in matching:
            for k, v in m['parsers'].items():
                if as_doc['parser_versions'].get(k, None) == v:
                    # Still pending our version
                    finished.remove(m['_id'])
                    break

        if finished:
            col_ids.update_many({'_id': {'$in': list(finished)}},
                    {'$set': {'done_parse': True}})

        # Give CPU a break if we're not making significant progress
        if len(finished) <= len(ids) // 2:
            dask.distributed.secede()
            time.sleep(2)
            dask.distributed.rejoin()

    # Finally, assign `parser_versions_done` so we don't re-run this set of
    # parsers if any other parsers change down the line.
    col_as_metadata.update_one({'_id': as_doc['_id']},
            {'$set': {'parser_versions_done': as_doc['parser_versions']}})



def _as_populate_gather(as_doc, col_ids, col_dst):
    """Gather all parser information specified in as_doc into `col_dst` from
    `col_ids`. Must be re-entrant.
    """
    while True:
        if faw_internal_util.dask_check_if_cancelled():
            break

        # Sample a batch to be done
        batch_size = 64
        ids = [d['_id'] for d in col_ids.find({'done': False}, {})
                .limit(batch_size)]
        if not ids:
            # Done!
            return

        # Again, $out sometimes broken. Anyway, idea is to have aggregation
        # pipeline do all the work, and then spool the documents out. We do this
        # one batch at a time, ignoring those we've already done.
        stages = []
        parsers = {}
        for r in as_doc['definition']['rules']:
            p = r['parser']
            if p not in parsers:
                parsers[p] = []
            parsers[p].append(r)
        stages.append({'$match': {'_id': {'$in': ids}}})
        for p in parsers.keys():
            stages.append({'$lookup': {
                    'from': 'invocationsparsed',
                    'let': {'id': '$_id'},
                    'pipeline': [
                        {'$match': {'$expr': {'$and': [
                            {'$eq': ['$file', '$$id']},
                            {'$eq': ['$parser', p]},
                        ]}}},
                    ],
                    'as': p,
            }})

        batch_out = []
        for doc in col_ids.aggregate(stages):
            doc_out = {'_id': doc['_id'], 'f': []}
            for p, rules in parsers.items():
                if not doc[p]:
                    # Parser not found
                    continue

                if len(doc[p]) > 1:
                    print(f'Parser {p} had more than one doc?', file=sys.stderr)

                pout = {}
                for fkv in doc[p][0]['result']:
                    fk = fkv['k']
                    fv = fkv['v']

                    if fk.startswith('<<workbench: Exit code'):
                        # Parser included, include this
                        pout[fk] = fv
                        continue

                    for r in rules:
                        # Trailing .* needed for feature substitution
                        rsrc = '^' + r['src'] + '.*'
                        rflags = 0 if r.get('src_case') else re.I
                        if re.search(rsrc, fk, flags=rflags) is None:
                            continue

                        if r['dst']:
                            fk = re.sub(rsrc, r['dst'], fk, flags=rflags)
                        # May collapse multiple features -- take last value
                        pout[fk] = fv
                        break

                for fk, fv in pout.items():
                    doc_out['f'].append({'k': p + '_' + fk, 'v': fv})
            batch_out.append(doc_out)

        # Ensure it will go through if a past operation has partially succeeded
        col_dst.delete_many({'_id': {'$in': ids}})
        col_dst.insert_many(batch_out)
        col_ids.update_many({'_id': {'$in': ids}}, {'$set': {'done': True}})


def _as_populate_ids(as_name, mongo_info, app_config):
    # Convergent
    db = faw_internal_util.mongo_api_info_to_db_conn(mongo_info)

    col_name = f'as_i_{as_name}'
    as_create_id_collection(db, as_name, col_name)

    # Reset 'done' markers, add indices
    col_ids = db[col_name]
    col_ids.create_index([('done', 1)])
    col_ids.create_index([('done_parse', 1)])
    col_ids.update_many({}, {'$set': {'done': False, 'done_parse': False}})

    # Gather parser versions
    col_as_metadata = db['as_metadata']
    as_doc = col_as_metadata.find_one({'_id': as_name})
    pipes = faw_analysis_set_util.aset_pipeline_parsers(app_config, as_doc)
    parser_versions_all = {}
    for pipe_aset_name, pipe_pipes in pipes.items():
        d = col_as_metadata.find_one({'_id': pipe_aset_name})
        if d is None:
            # Deleted the reliant analysis set
            continue

        for p, p_parser in pipe_pipes:
            p_done = d.get('pipelines', {}).get(p, {}).get('done')
            p_name = faw_analysis_set_util.lookup_pipeline_parser_name(
                    pipe_aset_name, p, p_parser)
            if p_done is not None:
                versions = faw_analysis_set_util.lookup_pipeline_parser_versions(
                        app_config, p, p_parser, p_done)
                parser_versions_all[p_name] = versions

    # Finally, update the analysis set metadata with all parser versions that we
    # Found at processing start time.
    # Note that this will always lead to eventual consistency -- if this aset is
    # using the wrong parser, it will be marked stale and restarted with the
    # right parser version, even if it previously overwrote the correct parser
    # version with a wrong one for each file it applies to.
    col_as_metadata.update_one({'_id': as_name},
            {'$set': {'parser_versions': parser_versions_all}})


def as_create_id_collection(db, aset_id, col_name, disable_sampling=False):
    """Create a brand new ID table named `col_name`. If it exists, delete it and
    re-populate. This table will contains documents with only an ``_id`` field
    corresponding to matches with the specified analysis set.

    Args:

    aset: ``as_metadata`` document for set to translate.

    disable_sampling: Set to ``True`` to ignore the "max documents" specified
            as part of the analysis set.
    """
    if col_name in db.list_collection_names():
        db.drop_collection(col_name)

    # $out would work well, but seems.. broken? So use a temporary...
    tmp_col = db['as_itmp_' + col_name]
    tmp_col.drop()

    aset = db['as_metadata'].find_one({'_id': aset_id})
    features = aset['definition'].get('features', [])
    cursor = None  # Iterator for documents with only '_id' field

    count_map = []
    if features:
        # MongoDB is terrible at joins. It has no query optimizer. We have to
        # do it manually.
        for fi, f in enumerate(features):
            fr = {}
            fr['name'] = f'f{fi}'
            fr['query'] = {
                    'parser': f['parser'],
                    # Don't use an re object -- breaks on $lookup
                    'result.k': {'$regex': '^' + f['ft'],
                        '$options': '' if f.get('ft_case') else 'i'},
            }
            fr['count'] = db['invocationsparsed'].count_documents(
                    fr['query'])
            count_map.append(fr)

        # Start with most constrained (AND assumption)
        count_map = sorted(count_map, key=lambda m: m['count'])

    idx_query = {'queueStop': {'$ne': None}, 'queueErr': None}
    if aset['definition']['files']:
        idx_query['_id'] = {'$regex': '^' + aset['definition']['files'],
                '$options': '' if aset['definition']['files_case'] else 'i'}
    count_idx = db['observatory'].count_documents(idx_query)
    sz = aset['definition']['sample']
    if disable_sampling:
        sz = 0

    if count_map:
        # Fancy
        col = db['invocationsparsed']
        stages = []
        idx_first = (count_idx < count_map[0]['count'])
        if idx_first:
            # Index first
            idx_first = True
            col = db['observatory']
            stages.append({'$match': idx_query})
            # For parity with invocationsparsed schema
            stages.append({'$addFields': {'file': '$_id'}})

        # Features
        if not idx_first:
            stages.append({'$match': count_map[0]['query']})
            count_map = count_map[1:]
        for c in count_map:
            query_not_expr = {k: v for k, v in c['query'].items()
                    if k != 'parser'}
            stages.append({'$lookup': {
                'from': 'invocationsparsed',
                'let': {'file': '$file'},
                'pipeline': [
                    {'$match': {'$expr': {'$and': [
                        {'$eq': ['$file', '$$file']},
                        {'$eq': ['$parser', c['query']['parser']]},
                    ]}}},
                    {'$match': query_not_expr},
                ],
                'as': c['name']
            }})
            stages.append({'$match': {c['name']: {'$ne': []}}})

        if not idx_first:
            stages.append({'$lookup': {
                'from': 'observatory',
                'let': {'file': '$file'},
                'pipeline': [
                    {'$match': {'$expr': {'$and': [
                        {'$eq': ['$_id', '$$file']},
                        {'$ne': ['$queueStop', None]},
                        {'$eq': ['$queueErr', None]},
                    ]}}},
                ],
                'as': 'idx',
            }})
            stages.append({'$match': {'idx': {'$ne': []}}})

        stages.append({'$replaceRoot': {'newRoot': {'_id': '$file'}}})
        cursor = col.aggregate(stages)
    else:
        # Sample only from documents that are completed, though.
        stages = []
        stages.append({'$match': idx_query})
        if sz > 0 and count_idx > sz:
            # Optimization -- let mongo do this work
            stages.append({'$sample': {'size': sz}})
        stages.append({'$project': {'_id': True}})
        cursor = db['observatory'].aggregate(stages)

    col_created = False
    batch = []
    n_inserted = 0  # Reservoir sampling
    n_seen = 0
    to_delete = 0
    def batch_apply():
        nonlocal col_created, to_delete
        tmp_col.insert_many(batch)
        col_created = True
        batch.clear()
        if to_delete > 0:
            # Delete randomly... Imperfect reservoir, but more efficient
            ids = tmp_col.aggregate([
                {'$sample': {'size': to_delete}},
                {'$project': {'_id': True}}])
            tmp_col.delete_many({'_id': {'$in': [i['_id'] for i in ids]}})
            to_delete = 0
    for doc in cursor:
        n_seen += 1
        if sz <= 0 or n_inserted < sz or random.randrange(n_seen) < sz:
            n_inserted += 1
            batch.append(doc)

            if sz > 0 and n_inserted > sz:
                to_delete += 1

        if len(batch) == 1024:
            batch_apply()
    if batch:
        batch_apply()

    # Finally, rename, but only if created (otherwise Mongo error)
    if col_created:
        tmp_col.rename(col_name)

