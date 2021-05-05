# Format Analysis Workbench (FAW)

The Format Analysis Workbench (FAW) is a platform for running and analyzing the
output from any number of parsers dealing with a single file format. It is meant
to be a workbench for developing tools which aid in understanding the de facto
formats which naturally emerge from open standards.

## Example Screenshots

Main overview of decisions
![Main overview](./common/readme/screenshot-main.png)

Decision DSL excerpt
![DSL excerpt](./common/readme/screenshot-dsl.png)

## Getting Started

To get started, run:

    pip3 install -r requirements.txt
    python3 workbench.py pdf ./test_files/pdf

The first invocation will take a long time (up to an hour or two). After that,
it will be quite fast. Once it's loaded, point your browser at
http://localhost:8123

## Creating a new distribution

This README documents the Galois Format Analysis Workbench (GFAW) for developers
creating their own distributions of the workbench. The core workbench source
in `common` should remain unchanged, but distribution creators are encouraged to
develop their own plugins to support new file formats or enhance support for
existing formats.

To create a new distribution, create a new subfolder, modeled off of the `pdf`
base distribution. Available options are documented in `config.json5` within
that folder.

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

--development live-reloads all plugins, including parsers, file views, and
decision views. For file or decision views, simply re-run the plugin from the
UI. For plugins, edit `config.json5` to change the `version` field of the
modified parser. Then, in the UI, press "Reset DB -> Reset Most..." to trigger
the re-execution of that parser.

To build a standalone workbench in `build/label`, run:

    python3 workbench.py pdf build/label

See `common/README-dist.md` for additional information on running the workbench
docker image in a standalone fashion; that file is packaged with any builds.

## Troubleshooting

1. Should `--development` fail to start the web interface, try deleting
  the `common/pdf-observatory/ui/node_modules` directory and trying again. This
  can happen due to mismatches between node versions in docker containers.

2. Additional documentation is in the [docs](docs) directory.

## Acknowledgements

This material is based upon work supported by the Defense Advanced Research 
Projects Agency (DARPA) under Contract No. HR0011-19-C-0073. Any opinions, 
findings and conclusions or recommendations expressed in this material are those 
of the author(s) and do not necessarily reflect the views of the Defense Advanced 
Research Projects Agency (DARPA).
