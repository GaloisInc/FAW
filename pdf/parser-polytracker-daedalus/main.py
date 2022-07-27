import os
from pathlib import Path
import signal
import sys
import subprocess
from tempfile import NamedTemporaryFile
from typing import Optional

from dumptdag import cavity_detection


def process_pdf(path: str, timeout: Optional[float] = None):
    # Prevent SIGTERM from having us not clean up our files
    def sigterm_handler(_signo, _sf):
        sys.exit(-1)
    signal.signal(signal.SIGTERM, sigterm_handler)

    with NamedTemporaryFile("wb", delete=False) as tmpfile, NamedTemporaryFile("wb", delete=False) as psfile:
        try:
            db_path = tmpfile.name
            ps_path = psfile.name
            tmpfile.close()
            psfile.close()
            subprocess.check_call(["/usr/bin/parser-test-track", f"--output-file={ps_path}", path], env={
                "POLYDB": db_path
            }, timeout=timeout)
            cavity_detection(Path(db_path), Path(path))
        except TimeoutError:
            sys.stderr.write(f"DaeDaLus timed out after {timeout} seconds")
        except subprocess.CalledProcessError as e:
            sys.stderr.write(f"DaeDaLus exited with code {e.returncode}")
        finally:
            os.unlink(tmpfile.name)
            os.unlink(psfile.name)


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser()
    parser.add_argument("INPUT_PATH", help="path to the PDF to analyze")
    parser.add_argument("--timeout", "-t", type=float, default=None, help="timeout in seconds")

    args = parser.parse_args()

    process_pdf(args.INPUT_PATH, timeout=args.timeout)
