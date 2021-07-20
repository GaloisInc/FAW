
import faw_internal_util

import asyncio
import dask.distributed
import enum
import random
import re
import sys
import time
import traceback

class AsState(enum.Enum):
    UP_TO_DATE = ''
    STALE = 'stale'  # Like up-to-date, but parsers ran after this. UI only
    REBUILD = 'rebuild'
    REBUILDING = 'rebuilding'
    DELETE = 'delete'

async def main_loop(app_mongodb_conn, app_config, get_api_info):
    global _app_mongodb_conn, _aset_conn_info
    _app_mongodb_conn = app_mongodb_conn
    conn_info = get_api_info()['mongo']

    client = await dask.distributed.Client('localhost:8786', asynchronous=True)
    client_tasks = {}
    while True:
        try:
            last_queueStop = await _as_last_queueStop()

            promises = []
            async for aset in _app_mongodb_conn['as_metadata'].find():
                promises.append(_as_manage(aset, conn_info, last_queueStop,
                        client, client_tasks))
            # Either way, let dask forget about defunct tasks
            new_tasks = {}
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
                definition=a['definition'])

        if adoc['status'] == AsState.UP_TO_DATE.value:
            if a['last_queueStop'] != last_queueStop:
                adoc['status'] = AsState.STALE.value

        r.append(adoc)
    return r


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

async def _as_manage(aset, mongo_info, last_queueStop, client, client_tasks):
    name = aset['_id']

    old_task_info = client_tasks.get(name)
    old_task_result = None
    if old_task_info is not None:
        try:
            old_task_result = await old_task_info[1].result(timeout=0.5)
        except dask.distributed.TimeoutError:
            pass
        except:
            # Another exception -- still pass, but unset the old task
            traceback.print_exc()
            old_task_info = None

    if aset['status'] == AsState.UP_TO_DATE.value:
        # See if stale
        if aset['last_queueStop'] != last_queueStop:
            # Rebuild, but only if processing has halted
            d = await _app_mongodb_conn['observatory'].find_one({
                    'queueStop': None})
            if d is None:
                # Everything is done. Set to rebuild
                print(f'analysis_set: Rebuilding {name} due to staleness')
                await _app_mongodb_conn['as_metadata'].update_one({'_id': name},
                        {'$set': {'status': AsState.REBUILD.value}})
        return

    if aset['status'] == AsState.DELETE.value:
        # Internal delete -- drop all associated collections
        if old_task_info is not None:
            # Wait for task to exit
            return

        # Give task time to exit
        await asyncio.sleep(5)
        await _app_mongodb_conn['as_c_' + name].drop()
        await _app_mongodb_conn['as_i_' + name].drop()
        await _app_mongodb_conn['as_metadata'].delete_one({'_id': name})
        return

    if aset['status'] == AsState.REBUILD.value:
        await _app_mongodb_conn['as_c_' + name].drop()
        last_queueStop = await _as_last_queueStop()

        # For now, always delete / regenerate index. To not do so, just comment
        # this line.
        await _app_mongodb_conn['as_i_' + name].drop()
        await _as_populate_ids(aset, name)

        await _app_mongodb_conn['as_metadata'].update_one(
                {'_id': name, 'status': AsState.REBUILD.value},
                {'$set': {'status': AsState.REBUILDING.value,
                    'last_queueStop': last_queueStop}})
        aset['status'] = AsState.REBUILDING.value

    if aset['status'] == AsState.REBUILDING.value:
        if old_task_info is not None and old_task_info[0] == AsState.REBUILDING:
            if old_task_result:
                # Finished with this step
                await _app_mongodb_conn['as_metadata'].update_one({'_id': name},
                        {'$set': {'status': AsState.UP_TO_DATE.value}})
                return
            else:
                # Keep old task alive
                return {name: old_task_info}
        future = client.submit(_as_populate, name, mongo_info,
                pure=False)
        return {name: (AsState.REBUILDING, future)}


def _as_populate(as_name, mongo_info):
    # Convergent
    db = faw_internal_util.mongo_api_info_to_db_conn(mongo_info)
    as_doc = db['as_metadata'].find_one({'_id': as_name})

    col_ids = db['as_i_' + as_name]
    col_dst = db['as_c_' + as_name]
    while True:
        if faw_internal_util.dask_check_if_cancelled():
            break

        # Sample a batch to be done
        batch_size = 64
        ids = [d['_id'] for d in col_ids.find({'done': False}, {})
                .limit(batch_size)]
        if not ids:
            # Done!
            return True

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


async def _as_populate_ids(aset, as_name):
    """Rebuild id table, if needed.
    """
    db = _app_mongodb_conn

    # Check progress
    doc = await db['as_metadata'].find_one({'_id': as_name})
    if 'as_i_' + as_name not in await db.list_collection_names():
        # No indices yet... sample from db.
        # $out would work well, but seems.. broken? So use a temporary...
        tmp_col = db['as_itmp_' + as_name]
        await tmp_col.drop()

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
                fr['count'] = await db['invocationsparsed'].count_documents(
                        fr['query'])
                count_map.append(fr)

            # Start with most constrained (AND assumption)
            count_map = sorted(count_map, key=lambda m: m['count'])

        idx_query = {'queueStop': {'$ne': None}, 'queueErr': None}
        if aset['definition']['files']:
            idx_query['_id'] = {'$regex': '^' + aset['definition']['files'],
                    '$options': '' if aset['definition']['files_case'] else 'i'}
        count_idx = await db['observatory'].count_documents(idx_query)
        sz = doc['definition']['sample']

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
        async def batch_apply():
            nonlocal col_created, to_delete
            await tmp_col.insert_many(batch)
            col_created = True
            batch.clear()
            if to_delete > 0:
                # Delete randomly... Imperfect reservoir, but more efficient
                ids = await tmp_col.aggregate([
                    {'$sample': {'size': to_delete}},
                    {'$project': {'_id': True}}]).to_list(None)
                await tmp_col.delete_many({'_id': {'$in': [i['_id'] for i in ids]}})
                to_delete = 0
        async for doc in cursor:
            n_seen += 1
            if sz <= 0 or n_inserted < sz or random.randrange(n_seen) < sz:
                n_inserted += 1
                batch.append(doc)

                if sz > 0 and n_inserted > sz:
                    to_delete += 1

            if len(batch) == 1024:
                await batch_apply()
        if batch:
            await batch_apply()

        # Finally, rename, but only if created (otherwise Mongo error)
        if col_created:
            await tmp_col.rename('as_i_' + as_name)

    # Reset 'done' markers
    col_ids = db['as_i_' + as_name]
    await col_ids.create_index([('done', 1)])
    await col_ids.update_many({}, {'$set': {'done': False}})

