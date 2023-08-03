
import schema

def schema_get():
    """Returns the schema for the "parsers" node in config.json5.
    """
    s = schema
    sch = s.Schema(s.Or({}, {
            s.And(str, lambda x: '_' not in x): {
                s.Optional('disabled', default=False): s.Or(True, False),
                s.Optional('mustSucceed', default=False): s.Or(True, False),
                'exec': [str],
                s.Optional('cwd', default='/home/dist'): str,
                s.Optional('timeout', default=None): s.Or(float, int, None),
                'version': str,
                'parse': schema_get_parser_parser(),
                s.Optional('devmount_dependencies'): [str],
            },
    }))
    return sch


def schema_get_parser_parser():
    s = schema
    regex_counter_stream_handler_type = s.Or({str: {
        s.Optional('nameGroup', default=0): s.Or(int, str),
        s.Optional('nameReplace', default={}): s.Or({str: str}, {}),
        s.Optional('countGroup', default=0): s.Or(int, str),
        s.Optional('countReplace', default={}): s.Or({str: str}, {}),
        s.Optional('countAsMissing', default=[]): [str],
        s.Optional('countAsNumber', default=False): bool,
        s.Optional('fallthrough', default=False): bool,
    }}, {})
    return s.Or(
            # Run an external program with the stdout+stderr, which
            # should output a json-encoded object containing all features.
            # PARSER first as "||FAW-PARSER <<parser>>\n"
            # STDOUT started as "||FAW-STDOUT\n"; STDERR as "\n||FAW-STDERR\n"
            # Note that 'cwd' is taken from parent category.
            {'type': 'program-stdin', 'exec': [str],
                'version': str,
            },

            # Run a custom, limited program to get counts.
            {'type': 'regex-counter',
                'version': str,
                s.Optional('stdstar', default={}): regex_counter_stream_handler_type,
                s.Optional('stdout', default={}): regex_counter_stream_handler_type,
                s.Optional('stderr', default={}): regex_counter_stream_handler_type,
            },
    )

