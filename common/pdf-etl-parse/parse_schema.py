
import schema

def schema_get():
    """Returns the schema for the "parsers" node in config.json5.
    """
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
    sch = s.Schema({
            s.And(str, lambda x: '_' not in x): {
                s.Optional('disabled', default=False): s.Or(True, False),
                'exec': [str],
                s.Optional('cwd', default='.'): str,
                s.Optional('timeout', default=None): s.Or(float, int, None),
                'version': str,
                'parse': s.Or(
                    # Run an external program with the stdout+stderr, which
                    # should output a json encoding
                    {'type': 'program', 'exec': [str]},

                    # Run a custom, limited program to get counts.
                    {'type': 'regex-counter',
                        s.Optional('stdstar', default={}): regex_counter_stream_handler_type,
                        s.Optional('stdout', default={}): regex_counter_stream_handler_type,
                        s.Optional('stderr', default={}): regex_counter_stream_handler_type,
                    },
                ),
            },
    })
    return sch

