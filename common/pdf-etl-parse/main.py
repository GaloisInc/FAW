# Local imports
import mongo_queue_helper
import parse_schema

# System imports
import click
import functools
import logging
import pyjson5
import re

QUEUE_PROCESSING_TIMEOUT = 60
"""Max seconds a parser might be active.  This should be several standard
deviations out.
"""

@functools.wraps(click.option)
def click_option(*args, **kwargs):
    defaults = {'show_default': True}
    defaults.update(kwargs)
    return click.option(*args, **defaults)


@click.command()
@click_option('-s', '--db-src', help="Source collection, with raw invocations",
        default='localhost:27017/littlegovdocs/rawinvocations')
@click_option('-q', '--db-queue', help="Queue collection (temporary)",
        default='localhost:27017/littlegovdocs/rawinvocations_queue')
@click_option('-d', '--db-dst', help="Dest collection, with parsed invocations",
        default='localhost:27017/littlegovdocs/invocationsparsed')
@click_option('-p', '--processes', default=0, help="Number of processes to "
        "spin up, or 0 for number of cores.")
@click_option('-t', '--threads', default=2, help="Number of threads per "
        "process.  For keeping consumer busy during network traffic.")
@click_option('--clean/--no-clean', default=False, help="Clean queue first, "
        "so all items are re-tried.")
@click_option('--live-mode/--no-live-mode', default=False,
        help="Live mode useful for large datasets; script will continuously "
        "scan for changes after initial finish.")
@click_option('--run-one', default=None, type=str, help="Specify to run only "
        "one document.")
@click_option('--config', required=True, help="Path to config specifying how "
        "parsers should be handled.")
def main(live_mode, processes, threads, clean, db_src, db_queue, db_dst,
        run_one, config):
    """
    Consumer for the queue of raw invocation data.
    """
    logging.basicConfig(level=logging.DEBUG)
    parsers_config = config_load(config)
    mongo_queue_helper.run(live_mode=live_mode, processes=processes,
            threads=threads, clean=clean, db_src=db_src, db_queue=db_queue,
            processing_timeout=QUEUE_PROCESSING_TIMEOUT,
            handle_clean_callback=functools.partial(handle_clean, db_dst=db_dst),
            handle_queue_callback=functools.partial(handle_doc,
                fname_rewrite=None, db_dst=db_dst,
                parsers_config=parsers_config),
            run_one=run_one)


def config_load(config):
    """Loads the given config file, and puts it in the expected schema.
    """
    app_config = pyjson5.load(open(config))
    return config_schema(app_config['parsers'])


def config_schema(config):
    """Takes a 'parsers' node from config, and applies the expected schema.
    """
    sch = parse_schema.schema_get()
    return sch.validate(config)


def handle_clean(conn_resolver, db_dst):
    conn_resolver(db_dst).drop()


def handle_doc(doc, conn_resolver, *, db_dst, fname_rewrite, parsers_config):
    """Parse this document."""
    conn_dst = conn_resolver(db_dst)

    inv_name = doc['invoker']['invName']
    cfg = parsers_config[inv_name]
    parse_version = cfg['parse']['version']

    try:
        inv_version = doc['invoker']['version']
        if cfg['version'] != inv_version:
            raise ValueError(f'Expected version {cfg["version"]}; tool ran as '
                    f'version {inv_version}')

        parse_cfg = cfg['parse']
        parse_fts = {}
        def clean_stream(s):
            s = re.sub(r'/home/pdf-files/[a-zA-Z0-9_./-]+', '', s)
            return s
        doc_stdout = clean_stream(doc['result'].get('stdoutRes', ''))
        doc_stderr = clean_stream(doc['result'].get('stderrRes', ''))
        if parse_cfg['type'] == 'regex-counter':
            parse_cfg_both = parse_cfg['stdstar']
            for sname, s, r in [('stdout', doc_stdout, parse_cfg['stdout']),
                    ('stderr', doc_stderr, parse_cfg['stderr'])]:
                for line in s.split('\n'):
                    if not line.strip():
                        continue

                    items = list(r.items()) + list(parse_cfg_both.items())

                    for r_regex, r_result in items:
                        m = re.search(r_regex, line)
                        if m is None:
                            continue

                        def text_from_spec(spec, replace):
                            if isinstance(spec, int):
                                r = m.group(spec)
                                if r is None:
                                    r = ''
                            else:
                                assert isinstance(spec, str), spec
                                r = m.expand(spec)

                            for kk, vv in replace.items():
                                r = re.sub(kk, vv, r)

                            # Don't include extraneous spaces, rather than
                            # placing that burden on the regex writer.
                            return r.strip()
                        name = text_from_spec(r_result['nameGroup'],
                                r_result['nameReplace'])
                        if not name:
                            if r_result['fallthrough']:
                                continue
                            break

                        count = text_from_spec(r_result['countGroup'],
                                r_result['countReplace'])

                        count_val = count.lower().strip()
                        if count_val and count_val not in r_result['countAsMissing']:
                            if r_result['countAsNumber']:
                                # Errors aren't well-tolerated here (2020-09-28),
                                # so instead be safe and aggregate into a few
                                # columns.
                                is_nan = False
                                try:
                                    cv = float(count_val)
                                except ValueError:
                                    is_nan = True
                                    cv = 0.
                                parse_fts[name] = parse_fts.get(name, 0) + 1
                                parse_fts[name + '_sum'] = parse_fts.get(name + '_sum', 0) + cv
                                parse_fts[name + '_nan'] = parse_fts.get(name + '_nan', 0) + (1 if is_nan else 0)
                            else:
                                # Not a number, count only
                                if count_val not in r_result['countAsMissing']:
                                    parse_fts[name] = parse_fts.get(name, 0) + 1

                        if r_result['fallthrough']:
                            continue
                        break
                    else:
                        placeholder = f'<<workbench: unhandled {sname}>> '
                        placeholder += re.sub('[0-9]', '', line.strip())
                        parse_fts[placeholder] = (
                                parse_fts.get(placeholder, 0) + 1)
        elif parse_cfg['type'] == 'program':
            # Validate exec, ensure the <stdinType> gets used.
            raise NotImplementedError('Stream stdout / stderr to program')
        else:
            raise NotImplementedError(parse_cfg['type'])

        # Add exit information
        if doc['result'].get('_cons', '').lower() in ('timeout', 'runtimeerror'):
            parse_fts['<<workbench: Exit code missing>>'] = 1
            parse_fts[f"<<workbench: Exit status: {doc['result']['_cons']}>>"] = 1
            exitcode = 'missing'
        else:
            exitcode = int(doc['result']['exitcode'])
            parse_fts[f"<<workbench: Exit code {exitcode}>>"] = 1

        # Quick audit over mongo disallowed keys for sending data from db
        # to user interface.
        for k in parse_fts.keys():
            if '\0' in k:
                raise ValueError(f'Null byte in: {k}')

        # Now that all features are collected, collapse them into a mongodb
        # format which can be queried.
        parse_fts = [{'k': k, 'v': v} for k, v in parse_fts.items()]

        d = {
                '_id': doc['_id'],
                'file': fname_rewrite or doc['file'],
                'parser': doc['invoker']['invName'],
                'version_parse': parse_version,
                'version_tool': inv_version,
                'result': parse_fts,
                'exitcode': exitcode,
        }
        conn_dst.replace_one({'_id': d['_id']}, d, upsert=True)
    except:
        raise ValueError(f'While parsing {inv_name} for {doc["file"]}')


if __name__ == '__main__':
    main()

