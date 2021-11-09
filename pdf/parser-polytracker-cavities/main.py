import os
import subprocess
from tempfile import NamedTemporaryFile

from polytracker import PolyTrackerTrace
from polytracker.mapping import InputOutputMapping


def process_pdf(path: str):
    with NamedTemporaryFile("wb", delete=False) as tmpfile, NamedTemporaryFile("wb", delete=False) as pngfile:
        try:
            db_path = tmpfile.name
            png_path = pngfile.name
            tmpfile.close()
            pngfile.close()
            subprocess.check_call(["/usr/bin/mutool_track_no_control_flow", "draw", "-o", png_path, path], env={
                "POLYPATH": path,
                "POLYDB": db_path
            })
            trace = PolyTrackerTrace.load(db_path)
            for cavity in InputOutputMapping(trace).file_cavities():
                print(f"{cavity.source.path}\t{cavity.offset}\t{cavity.offset + cavity.length - 1}")
        finally:
            os.unlink(tmpfile.name)
            os.unlink(pngfile.name)


if __name__ == "__main__":
    import sys
    process_pdf(sys.argv[1])
