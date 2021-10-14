import os
import subprocess

from polytracker import PolyTrackerTrace


def process_pdf(path: str):
    db_path = "polytracker.db"
    if os.path.exists(db_path):
        os.unlink(db_path)
    subprocess.check_call(["/usr/bin/mutool_track", "info", path], env={
        "POLYPATH": path,
        "POLYDB": db_path
    })
    trace = PolyTrackerTrace.load(db_path)
    for event in trace:
        # this prints every single event in the trace, in order
        print(event)


if __name__ == "__main__":
    import sys
    process_pdf(sys.argv[1])
