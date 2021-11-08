import os
import subprocess
from tempfile import NamedTemporaryFile

from polytracker import PolyTrackerTrace


def process_pdf(path: str):
    with NamedTemporaryFile("wb", delete=False) as tmpfile:
        try:
            tmpfile.close()
            db_path = tmpfile.name
            subprocess.check_call(["/usr/bin/mutool_track", "info", path], env={
                "POLYPATH": path,
                "POLYDB": db_path
            })
            trace = PolyTrackerTrace.load(db_path)
            for event in trace:
                # this prints every single event in the trace, in order
                print(event)
        finally:
            os.unlink(tmpfile.name)


if __name__ == "__main__":
    import sys
    process_pdf(sys.argv[1])
