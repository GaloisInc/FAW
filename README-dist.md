# Galois Format Analysis Workbench (GFAW): {distribution}

The workbench software is distributed as a Docker image, packaged with a
convenience script and this README.

## Requirements

The user's system must have Docker installed.

The convenience script additionally requires Python 3, and the user's system
must have the `docker` command available on the commandline.

## Convenience script

The convenience script, `workbench-{distribution}.py`, will automatically load the docker image, and delete the file on disk when it's done.  Therefore, to run the workbench using the convenience script, simply cd to this folder and run:

    python3 workbench-{distribution}.py path/to/files

Where `path/to/files` is the path to a folder containing files to be investigated.

The convenience script should automatically open http://localhost:8123 in your browser; if it doesn't, navigate to that address manually.

## Manual

If the convenience script is not trusted, feel free to use the image manually.

Install the image by running:

    docker load -i galois-workbench-{distribution}.image

Recommended is a command for persistent execution, which saves the database between runs:

    docker run -it --rm -p 8123:8123 \
      -v `readlink -f path/to/files`:/home/pdf-files \
      -v workbench-{distribution},/data/db \
      -e DB="DB_KEY" \
      {imageName}
    # where DB_KEY is a unique name for the database associated with path/to/PDFs.

For a one-off, transient execution of the workbench:

    docker run -it --rm -p 8123:8123 -v `readlink -f path/to/PDFs`:/home/pdf-files {imageName}

Either way, once the container is running, point your browser at http://127.0.0.1:8123
