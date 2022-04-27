# Local imports
import mongo_queue_helper
import parse_schema

# System imports
import click
import functools
import io
import json
import logging
import pyjson5
import re
import subprocess
import traceback

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


def handle_doc(doc, conn_resolver, *, db_dst, fname_rewrite, parse_version,
        parsers_config, parser_parsers_shared):
    """Parse this document."""
    conn_dst = conn_resolver(db_dst)

    inv_name = doc['invoker']['invName']
    cfg = parsers_config[inv_name]

    try:
        # Only check version of primary parser, not shared parser parsers
        inv_version = doc['invoker']['version']
        # Support old format by not assuming this
        if not isinstance(inv_version, dict):
            inv_version = {'': inv_version}
        version_ok = cfg['version'] == inv_version['']
        if 'pipeline' in cfg:
            version_ok = inv_version[''].startswith(cfg['version'])
        if not version_ok:
            raise ValueError(f'Expected version {cfg["version"]}; tool ran as '
                    f'version {inv_version}')

        def clean_stream(s):
            s = re.sub(r'/(home|tmp)/pdf-files/[a-zA-Z0-9_./-]+', '', s)
            return s
        doc_stdout = clean_stream(doc['result'].get('stdoutRes', ''))
        doc_stderr = clean_stream(doc['result'].get('stderrRes', ''))
        parser_list = [('', cfg)]
        for k, v in parser_parsers_shared.items():
            if v['disabled']:
                continue
            parser_list.append((k, v))

        parse_fts_all = {}
        for parser_name, parser in parser_list:
            parse_cfg = parser['parse']
            parse_fts = {}
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
                            # name CANNOT have null characters -- we could push this
                            # off to the config, but it's universal. So replace.
                            name = name.replace('\0', '<NUL>')
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
                            placeholder += re.sub('[0-9]+', '<INT>', line.strip())
                            parse_fts[placeholder] = (
                                    parse_fts.get(placeholder, 0) + 1)
            elif parse_cfg['type'] == 'program-stdin':
                # Validate exec, ensure the <stdinType> gets used.
                buf = io.BytesIO()
                buf.write(f'||FAW-PARSER {inv_name}\n'.encode())
                buf.write(b'||FAW-STDOUT\n')
                buf.write(doc_stdout.encode())
                buf.write(b'\n||FAW-STDERR\n')
                buf.write(doc_stderr.encode())
                buf.seek(0)
                p = subprocess.Popen(parse_cfg['exec'],
                        cwd=parser['cwd'],
                        stdin=subprocess.PIPE,
                        stdout=subprocess.PIPE,
                        stderr=subprocess.PIPE)
                p_out, p_err = p.communicate(buf.getvalue())
                p_res = p.wait()

                if p_res != 0:
                    raise ValueError(
                            f'{parse_cfg["exec"]} returned {p_res}:\n\n'
                            f'{p_err.decode()}')

                try:
                    parse_fts = json.loads(p_out.decode())
                except:
                    # Add some context
                    raise ValueError(p_out[:60])
            else:
                raise NotImplementedError(parse_cfg['type'])

            prefix = ''
            if parser_name:
                prefix = f'{parser_name}_'
            for k, v in parse_fts.items():
                if len(k) > 1024:
                    # This is just too long of a feature. Truncate
                    k = k[:1024] + '<<workbench: truncated>>'
                parse_fts_all[f'{prefix}{k}'] = v

        # Cast back to smaller name
        parse_fts = parse_fts_all

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
        # Dask doesn't show chained exceptions as of 2021-11-05
        raise ValueError(f'While parsing {inv_name} for {doc["file"]}:\n\n{traceback.format_exc()}')


if __name__ == '__main__':
    main()

