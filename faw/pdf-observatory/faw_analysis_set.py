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
_app_force_stale = {}  # {'aset': True} for asets which have been modified in such
                       # a way that their current processing should be aborted.

class AsStatus(enum.Enum):
    UP_TO_DATE = ''
    REBUILD_IDS = 'rebuild_ids'  # Re-samples indices to include in aset
    REBUILD_DATA = 'rebuild_data'  # Sets up metadata for distributed parsing/assembling
    REBUILDING = 'rebuilding'  # Triggers re-parsing of files with different
                               # versions, and assembling of data into the
                               # analysis set's document collection.
    COMPILING = 'compiling'
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

    # Porting old version of DB (pre-parser_parsers_shared) to new version
    asets = [('as_metadata', d)
            async for d in app_mongodb_conn['as_metadata'].find()]
    asets.append(('misc', await app_mongodb_conn['misc'].find_one({'_id': 'as_idle'})))
    for coll, doc in asets:
        if doc is None:
            # misc guard
            continue

        changes = {}
        # Check for old format of data -- parser_versions not a list
        if not isinstance(doc.get('parser_versions', []), list):
            changes['parser_versions'] = [None, None]
            changes['parser_versions_done'] = [None, None]
        else:
            # Check if parser versions are a single string instead of a dict of
            # versions
            for i, parser_set in enumerate(doc['parser_versions']):
                if parser_set is None:
                    continue
                for k, v in parser_set.items():
                    if not isinstance(v[0], dict):
                        v_new = {'': v[0]}
                        changes[f'parser_versions.{i}.{k}.0'] = v_new
                    if not isinstance(v[1], dict):
                        v_new = {'': v[1]}
                        changes[f'parser_versions.{i}.{k}.1'] = v_new
        if changes:
            # Old data format, time for a new one
            app_mongodb_conn[coll].update_one({'_id': doc['_id']},
                    {'$set': changes})

    client = None
    client_tasks = {}
    client_version = _app_config_version
    while True:
        try:
            if client is None:
                # This can fail sometimes; let it raise an error and try again
                client = await dask.distributed.Client('localhost:8786',
                        asynchronous=True)

            # Let dask forget about defunct tasks; they'll cancel themselves
            new_tasks = {}

            # When config goes out of date, kill everything and restart to
            # prevent version contamination.
            if client_version != _app_config_version:
                client_version = _app_config_version
                client_tasks = new_tasks
                await asyncio.sleep(1.)
                continue

            # Ensure we have a task that's kicking off parses
            parse_task = client_tasks.get('as_parse')
            if parse_task is not None:
                try:
                    await parse_task.result(timeout=0.5)
                except dask.distributed.TimeoutError:
                    pass
                except:
                    traceback.print_exc()
                    parse_task = None
            if parse_task is None:
                # Spin up
                new_tasks['as_parse'] = client.submit(
                            faw_analysis_set_parse.as_parse_main,
                            _app_config, api_info, priority=10000,
                            pure=False)
            else:
                new_tasks['as_parse'] = parse_task

            # For each analysis set, spin up a management task and wait for
            # completion.
            promises = []
            async for aset in _app_mongodb_conn['as_metadata'].find():
                promises.append(_as_manage(aset, api_info, client, client_tasks))
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
            'status': AsStatus.DELETE.value}})


async def as_list():
    """List non-deleted fields.
    """
    r = []
    async for a in _app_mongodb_conn['as_metadata'].find(
            {'status': {'$ne': AsStatus.DELETE.value}}).sort('_id'):
        name = a['_id']
        colname = f'as_c_{name}'
        col = _app_mongodb_conn[colname]
        stats = await _app_mongodb_conn.command('collstats', colname)

        adoc = dict(id=a['_id'], size_docs=await col.estimated_document_count(),
                size_disk=stats['storageSize'],
                status=a['status'], status_done_time=a.get('status_done_time'),
                definition=a['definition'], pipelines=a.get('pipelines', {}))

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
            'status': AsStatus.REBUILD_IDS.value
    }}, upsert=True)
    _app_force_stale[id] = True


########################################################
# Management code

'''
Internally, analysis sets are two collections plus a metadata document.

Metadata document: _id, definition, status. Maybe other fields:

Index collection: _id of files included in set.

Stats collection: old statsbyfile, but new schema.
'''

async def _as_manage(aset, api_info, client, client_tasks):
    name = aset['_id']
    mongo_info = api_info['mongo']

    # Forced staleness due to external modification
    if _app_force_stale.pop(name, False):
        return

    # Check old task status -- see if it finished
    old_task_info = client_tasks.get(name)
    missing = {}
    old_task_result = missing
    if old_task_info is not None:
        try:
            old_task_result = await old_task_info.result(timeout=0.5)
        except dask.distributed.TimeoutError:
            pass
        except:
            # Another exception -- still pass, but unset the old task
            traceback.print_exc()
            old_task_info = None

    # See if stale -- standard parsing queues. An analysis set is stale if
    # its parser versions are out of date. In that case, we want to
    # re-trigger tool runs to ensure we get the right versions. This can happen
    # mid-run, to prevent finishing a batch that will be stale when it
    # completes.
    loop = asyncio.get_running_loop()
    versions_id, versions_data = await loop.run_in_executor(None,
            faw_analysis_set_util.aset_parser_versions_calculate, _app_config,
                _app_mongodb_conn.delegate, aset)

    # Check for staleness -- old versions
    stale = False
    if versions_id != aset.get('parser_versions', [None, None])[0]:
        print(f'Rebuilding {name} for IDs due to staleness')
        stale = True
        await _app_mongodb_conn['as_metadata'].update_one({'_id': name},
                {'$set': {'status': AsStatus.REBUILD_IDS.value,
                    'parser_versions': [versions_id, versions_data]}})
    elif versions_data != aset.get('parser_versions', [None, None])[1] and (
            # Don't push past ID rebuilding on accident
            aset['status'] not in (AsStatus.REBUILD_IDS.value,)):
        print(f'Rebuilding {name} for data due to staleness')
        stale = True
        await _app_mongodb_conn['as_metadata'].update_one({'_id': name},
                {'$set': {'status': AsStatus.REBUILD_DATA.value,
                    'parser_versions': [versions_id, versions_data]}})

    if stale:
        # Ensure prior tasks get forgotten, so that new versions propagate
        return

    # Otherwise, check if existing task is completed / needs to be (re)started
    if aset['status'] == AsStatus.DELETE.value:
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
        await _app_mongodb_conn['as_itmpids_as_i_' + name].drop()
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

    if aset['status'] == AsStatus.UP_TO_DATE.value:
        # Nothing to do; no management process required
        return

    # Otherwise, trigger the management process
    if old_task_info is not None:
        if old_task_result is not missing:
            # Finished successfully; status should be updated, so this shouldn't
            # happen, but next loop will fix it.
            return

        # Keep old task alive
        return {name: old_task_info}

    # Launch a new task
    future = client.submit(_as_populate, name, mongo_info, _app_config,
            priority=10000, pure=False)
    return {name: future}


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
            old_task_result = await old_task_info.result(timeout=0.5)
        except dask.distributed.TimeoutError:
            pass
        except:
            traceback.print_exc()
            old_task_info = None

    if old_task_info is not None and old_task_result is missing:
        # Still running
        return {name: old_task_info}

    # New management needed
    # Import here to break cyclical dependency
    import faw_pipelines
    future = client.submit(faw_pipelines.pipeline_admin, _app_config,
            api_info, aset['_id'], priority=10000, pure=False)
    return {name: future}


def _as_populate(as_name, mongo_info, app_config):
    """Does all steps of rebuilding process, using 'status' to track where we're
    at.

    Generally:
        REBUILD_IDS -> REBUILD_DATA -> REBUILDING -> COMPILING -> UP_TO_DATE.

    Must be re-entrant.
    """
    # Convergent
    db = faw_internal_util.mongo_api_info_to_db_conn(mongo_info)
    col_as_metadata = db['as_metadata']
    as_doc = col_as_metadata.find_one({'_id': as_name})

    col_ids = db['as_i_' + as_name]
    col_parse = db[faw_analysis_set_parse.COL_NAME]
    col_dst = db['as_c_' + as_name]

    def update_status(status):
        """Call to move from current state in as_doc to next state.

        Returns False when computation should abort.
        """
        assert isinstance(status, str), status
        if faw_internal_util.dask_check_if_cancelled():
            return False

        update = {'status': status}
        if status == AsStatus.UP_TO_DATE.value:
            update['status_done_time'] = time.time()

        ostatus = as_doc['status']
        r = col_as_metadata.update_one({'_id': as_name, 'status': ostatus},
                {'$set': update})
        if r.modified_count == 0:
            # No document modified; status changed?
            return False

        as_doc.update(update)
        return True

    # Do NOT delete old result documents to avoid confusing the user. Now that
    # parsing is on-demand, and governed by the analysis sets, it makes sense
    # to keep old data around until we can actually replace it. This minimizes
    # user downtime.

    if as_doc['status'] == AsStatus.REBUILD_IDS.value:
        with dask.distributed.worker_client():  # Secede from dask for long op
            as_create_id_collection(db, app_config, as_name, col_ids.name)
            # MUST also clear out `parser_versions_done`! Otherwise, we may have
            # a new batch of files which are not guaranteed to be at the latest
            # version, and we may end up displaying old data.
            if as_doc.get('parser_versions_done'):
                r = col_as_metadata.update_one({'_id': as_name},
                        {'$set': {'parser_versions_done.1': {}}})
                if r.modified_count == 0:
                    raise ValueError(f'Unmodified? {as_name}')
                as_doc['parser_versions_done'][1] = {}
        if not update_status(AsStatus.REBUILD_DATA.value):
            return

    if as_doc['status'] == AsStatus.REBUILD_DATA.value:
        _as_populate_ids_setup_col(col_ids)
        if not update_status(AsStatus.REBUILDING.value):
            return

    ## Below this is AsStatus.REBUILDING
    if as_doc['status'] == AsStatus.REBUILDING.value:
        # Stage 1 -- run pipeline parsers on files as needed
        _, pv_data = as_doc['parser_versions']
        _, pv_data_done = as_doc.get('parser_versions_done', [{}, {}])
        with dask.distributed.worker_client():
            if not _as_populate_parsers(app_config, pv_data, pv_data_done,
                    col_ids, col_parse):
                # Failure -- abort without updating anything
                return
        # On completion, assign `parser_versions_done` so we don't re-run
        # this set of parsers if any other parsers change down the line.
        col_as_metadata.update_one({'_id': as_doc['_id']},
                {'$set': {'parser_versions_done': as_doc['parser_versions']}})

        if faw_internal_util.dask_check_if_cancelled():
            # One final check before purging previous data
            return

        # Delete original data immediately before setting status to COMPILING
        # which loads new results.
        col_dst.drop()
        if not update_status(AsStatus.COMPILING.value):
            return

    # Finally, compiling
    # Collect parser information
    _as_populate_gather(as_doc, col_ids, col_dst)

    # Declare done
    if not update_status(AsStatus.UP_TO_DATE.value):
        return


def _as_populate_parsers(app_config, parser_versions, parser_versions_done,
        col_ids, col_parse):
    """Scan through documents which require additional parsing. Use the version
    that was specified when the ids were populated so that we have a nice,
    uniform parse across all files.

    Must be re-entrant.

    Must happen outside of dask context.

    Returns `True` on successful completion, even if `parser_versions == parser_versions_done`.

    Note that this is used multiple places.
    """
    batch_size = 128

    # Stage 1
    # Some versions may not match. Loop through all documents, setting
    # `done_parse = 'maybe'` to indicate this. Only do this with documents
    # where `done_parse == False` for re-entrant optimization.

    # To minimize file transfers, add parsers required to each
    # document, and let another process run all parsers on a file in a single
    # batch.

    new_parsers = {}
    for k, v in parser_versions.items():
        if parser_versions_done.get(k) != v and v is not None:
            new_parsers[k] = v

    if not new_parsers:
        # Nothing to do
        return True

    # Priority of this parser is number of files times number of parsers
    # required.
    priority_order = (col_ids.estimated_document_count()
            * len(new_parsers))

    while True:
        if faw_internal_util.dask_check_if_cancelled():
            # Abort
            return
        ids = [d['_id'] for d in col_ids.find({'done_parse': False}, {})
                .limit(batch_size)]
        if not ids:
            break

        updates = [
                pymongo.UpdateOne({'_id': i},
                    {
                        '$set': {
                            'parsers.' + k: v for k, v in new_parsers.items()},
                        '$min': {
                            'priority_order': priority_order, },
                        # Whenever we get a new parser, it might fix an existing
                        # error. So, unset that error.
                        '$unset': {
                            'error_until': True,
                            'error_delay': True, },
                    },
                    upsert=True)
                for i in ids]
        col_parse.bulk_write(updates)

        # Success - onward!
        col_ids.update_many({'_id': {'$in': ids}}, {'$set': {
                'done_parse': 'maybe'}})

    # Stage 2 -- wait for all documents to complete all needed parsers
    while True:
        if faw_internal_util.dask_check_if_cancelled():
            # Abort
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
                if parser_versions.get(k, None) == v:
                    # Still pending our version
                    finished.remove(m['_id'])
                    break

        if finished:
            col_ids.update_many({'_id': {'$in': list(finished)}},
                    {'$set': {'done_parse': True}})

        # Give CPU a break if we're not making significant progress
        if len(finished) <= len(ids) // 2:
            # Remember -- we're seceded. Sleep is ok
            time.sleep(2)
    return True


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


def _as_populate_ids_setup_col(col_ids):
    """Given a collection containing IDs, analogous to that required for an
    analysis set, set markers required for `_as_populate_parsers`.
    """
    # Reset 'done' markers, add indices
    col_ids.create_index([('done', 1)])
    col_ids.create_index([('done_parse', 1)])
    col_ids.update_many({}, {'$set': {'done': False, 'done_parse': False}})


def as_create_id_collection(db, app_config, aset_id, col_name, *,
        disable_sampling=False):
    """Create a brand new ID table named `col_name`. If it exists, delete it and
    re-populate. This table will contains documents with only an ``_id`` field
    corresponding to matches with the specified analysis set.

    IMPORTANT: this may take an extraordinarily long time! If the analysis set
    has feature dependencies, then those parsers must be run on 100% of files in
    the database before this set can proceed.

    MAY call `faw_internal.dask_check_if_cancelled()` and return `None` for
    early exits. In this case, `col_name` will not exist.

    THEREFORE: SHOULD BE CALLED OUTSIDE OF DASK CONTEXT!

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

    # See if we have parser requirements for ID pool
    parsers_id, _ = aset['parser_versions']
    parsers_id_done, parsers_data_done = aset.get('parser_versions_done', [{}, {}])
    if parsers_id != parsers_id_done:
        # OK, we have to make sure that the requisite parsers are run on ALL
        # files in the database. So, simulate a transient analysis set.
        tmp_id_col = db['as_itmpids_' + col_name]
        tmp_id_col.drop()

        batch = []
        def batch_apply():
            if not batch:
                return
            tmp_id_col.insert_many(batch)
            batch.clear()
        for doc in db['observatory'].find({}, {}):
            batch.append(doc)
            if len(batch) >= 1024:
                batch_apply()
        batch_apply()
        _as_populate_ids_setup_col(tmp_id_col)

        # At this point, it's a pseudo-analysis set.
        col_parse = db[faw_analysis_set_parse.COL_NAME]
        if not _as_populate_parsers(app_config, parsers_id, parsers_id_done,
                tmp_id_col, col_parse):
            raise ValueError('_as_populate_parsers failed; reference lost?')

        # Finally, delete temporary, then set done
        tmp_id_col.drop()
        db['as_metadata'].update_one({'_id': aset['_id']},
                {'$set': {'parser_versions_done.0': parsers_id}})

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

    idx_query = {'queueErr': None}
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

