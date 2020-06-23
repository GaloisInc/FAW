This README documents the Galois Format Analysis Workbench (GFAW) for developers
creating their own distributions of the workbench. The core workbench source
in `common` should remain unchanged, but distribution creators are encouraged to
develop their own plugins to support new file formats or enhance support for
existing formats.

To create a new distribution, create a new subfolder, modeled off of the `pdf`
base distribution. Available options are documented in `config.json5` within
that folder.

Please do not add other files directly to this folder.

To get started, run:

    pip3 install -r requirements.txt
    python3 workbench.py pdf ./test_files/pdf

The first invocation will take a long time (up to an hour or two). After that,
it will be quite fast. Once it's loaded, point your browser at
http://localhost:8123

While developing a distribution, consider using `--development` to use Vue's
live reload functionality and to mount the distribution's folder into
`/home/` in a way which allows for making changes without stopping and starting
the development server.

To build a standalone workbench in `build/label`, run:

    python3 workbench.py pdf build/label

See `common/README-dist.md` for additional information on running the workbench
docker image in a standalone fashion; that file is packaged with any builds.

