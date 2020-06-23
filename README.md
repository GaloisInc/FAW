This README documents the Galois Format Analysis Workbench (GFAW) for developers
creating their own distributions of the workbench. The core workbench source
in `common` should remain unchanged, but distribution creators are encouraged to
develop their own plugins to support new file formats or enhance support for
existing formats.

To create a new distribution, create a new subfolder, modeled off of the `pdf`
base distribution. Available options are documented in `config.json5` within
that folder.

To get started, run:

    pip3 install -r requirements.txt
    python3 workbench.py pdf ./test_files/pdf

The first invocation will take a long time (up to an hour or two). After that,
it will be quite fast. Once it's loaded, point your browser at
http://localhost:8123

Please do not add other files directly to this folder. For development purposes,
such as keeping a private workbench distribution, the configuration for a
distribution might be kept in the parent folder to which the FAW is cloned.
E.g., one might run:

    python3 workbench.py ../my-pdf ./test_files/pdf

Due to the design of Docker, running this will upload all contents of `..` to
the Docker daemon, which may be time consuming and wasteful. If using this
feature, please ensure the parent folder is as empty as possible, preferentially
only containing the FAW folder and the private distribution. Note that multiple
cloned FAWs will mostly use the same Docker images, so no need to worry about the
inefficiency of cloning FAW multiple times. The exception is that two builds
will be maintained -- one for non-parent-directory distributions, and one for
parent-directory distributions.

While developing a distribution, consider using `--development` to use Vue's
live reload functionality and to mount the distribution's folder into
`/home/` in a way which allows for making changes without stopping and starting
the development server.

To build a standalone workbench in `build/label`, run:

    python3 workbench.py pdf build/label

See `common/README-dist.md` for additional information on running the workbench
docker image in a standalone fashion; that file is packaged with any builds.

