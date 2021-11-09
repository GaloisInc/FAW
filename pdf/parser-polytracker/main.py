import os
import sys
import subprocess
from tempfile import NamedTemporaryFile
from typing import Optional

from polytracker import PolyTrackerTrace
from polytracker.mapping import InputOutputMapping


def process_pdf(path: str, timeout: Optional[float] = None):
    with NamedTemporaryFile("wb", delete=False) as tmpfile, NamedTemporaryFile("wb", delete=False) as pngfile:
        try:
            db_path = tmpfile.name
            png_path = pngfile.name
            tmpfile.close()
            pngfile.close()
            subprocess.check_call(["/usr/bin/mutool_track_no_control_flow", "draw", "-o", png_path, path], env={
                "POLYPATH": path,
                "POLYDB": db_path
            }, timeout=timeout)
            trace = PolyTrackerTrace.load(db_path)
            for cavity in InputOutputMapping(trace).file_cavities():
                print(f"{cavity.source.path}\t{cavity.offset}\t{cavity.offset + cavity.length - 1}")
        except TimeoutError:
            sys.stderr.write(f"`mutool draw` timed out after {timeout} seconds")
        except subprocess.CalledProcessError as e:
            sys.stderr.write(f"`mutool draw` exited with code {e.returncode}")
        finally:
            os.unlink(tmpfile.name)
            os.unlink(pngfile.name)


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser()
    parser.add_argument("INPUT_PATH", help="path to the PDF to analyze")
    parser.add_argument("--timeout", "-t", type=float, default=None, help="timeout in seconds")

    args = parser.parse_args()

    process_pdf(args.INPUT_PATH, timeout=args.timeout)
