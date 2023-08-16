import os
from pathlib import Path
import signal
import sys
import subprocess
from tempfile import NamedTemporaryFile
from typing import List, Optional

from polytracker.clustering import load_indexes_for_matching, MultipleTaintSourcesError, ordered_edit_distance
from polytracker.clustering import match as cluster_match


def run_httpd(request_path: str, output_path: str, timeout: Optional[float] = None):
    try:
        subprocess.check_call(["/usr/bin/httpd_harness", request_path], env={
            "POLYDB": output_path,
            "POLYTRACKER_STDOUT_SINK": "1"
        }, timeout=timeout)
    except TimeoutError:
        sys.stderr.write(f"Apache timed out after {timeout} seconds")
    except subprocess.CalledProcessError as e:
        sys.stderr.write(f"Apache exited with code {e.returncode}")


def run_pico(request_path: str, output_path: str, timeout: Optional[float] = None):
    try:
        subprocess.check_call(["/usr/bin/picohttpparser_track", request_path], env={
            "POLYDB": output_path,
            "POLYTRACKER_STDOUT_SINK": "1"
        }, timeout=timeout)
    except TimeoutError:
        sys.stderr.write(f"Picohttp timed out after {timeout} seconds")
    except subprocess.CalledProcessError as e:
        sys.stderr.write(f"Picohttp exited with code {e.returncode}")


def load_index_preferring_socket(path: str) -> List[int]:
    try:
        try:
            return load_indexes_for_matching(Path(path))
        except MultipleTaintSourcesError as e:
            filtered_sources = {
                str(source_path)
                for source_path, _ in e.sources
                if str(source_path) != "/dev/stdout"
            }
            if len(filtered_sources) == 1:
                return load_indexes_for_matching(Path(path), from_source=next(iter(filtered_sources)))
            sockets = {
                source_path
                for source_path in filtered_sources
                if source_path.startswith("socket:")
            }
            if len(sockets) == 1:
                return load_indexes_for_matching(Path(path), from_source=next(iter(sockets)))
            else:
                msg = str(e).replace("\\n", "\n")[1:-1]
                sys.stderr.write(f"Error: {msg}\n")
                sys.stderr.write(
                    "\nThe trace has multiple taint sources and no unique sockets!\n"
                )
                exit(1)
    except (KeyError, ValueError) as e:
        msg = str(e).replace("\\n", "\n")[1:-1]
        sys.stderr.write(f"Error: {msg}\n")
        exit(1)


def match(path1: str, path2: str):
    index1 = load_index_preferring_socket(path1)
    index2 = load_index_preferring_socket(path2)
    sys.stderr.write("Matching...\n")
    m = cluster_match(index1, index2)
    for k, v in m.mapping.items():
        indexes1 = m.s1.indexes[k]
        indexes2 = m.s2.indexes[v]
        cost = ordered_edit_distance(
            indexes1, indexes2, infinite_cost=m.infinite_cost
        )
        if cost > 0:
            print(f"{indexes1} -> {indexes2} cost {cost}")
    print(f"cost={m.edit_distance}, similarity={m.similarity}")


def process_http(path: str, timeout: Optional[float] = None):
    # Prevent SIGTERM from having us not clean up our files
    def sigterm_handler(_signo, _sf):
        sys.exit(-1)
    signal.signal(signal.SIGTERM, sigterm_handler)

    with NamedTemporaryFile("wb", delete=False) as httpd, NamedTemporaryFile("wb", delete=False) as pico:
        try:
            httpd_db = httpd.name
            pico_db = pico.name
            httpd.close()
            pico.close()
            run_httpd(request_path=path, output_path=httpd_db, timeout=timeout)
            run_pico(request_path=path, output_path=pico_db, timeout=timeout)
            match(httpd_db, pico_db)
        finally:
            os.unlink(httpd.name)
            os.unlink(pico.name)


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser()
    parser.add_argument("INPUT_PATH", help="path to the HTTP request to analyze")
    parser.add_argument("--timeout", "-t", type=float, default=None, help="timeout in seconds")

    args = parser.parse_args()

    process_http(args.INPUT_PATH, timeout=args.timeout)
