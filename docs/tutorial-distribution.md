# Introduction

A FAW distribution is a specific configuration of the FAW. It is usually associated with a specific format; for example, the FAW repository contains a small set of distributions for file formats including one for PDFs.

In this tutorial, we will look at creating a new CSV distribution from scratch. Additional information can also be found by looking at how the example distributions have been put together; in particular, the corresponding `config.json5` files often contain comments about configuring/using particular features of the FAW. See for example the configuration files for [PDF](/pdf/config.json5) and [NITF](/nitf/config.json5) distributions.

# Getting Started

Distributions are typically created as separate folders within the FAW repository. These folders consist of a top-level `config.json5` file and various plugins. While it is conventionally organized with each plugin having its own subfolder and configuration file, these files can be merged with the main configuration. They can also define the process for building and installing dependencies required during runtime, which can range from simple system package installations to complex multi-stage builds. Moreover, the distribution image can be tailored to include only essential files and binaries, ensuring a clear separation between the plugin's build process and the necessary runtime artifacts.

A simple top-level configuration file for a simple CSV parser/validator looks like the below:

```json5
{
  // The name of the docker image to be produced.
  name: 'galois-workbench-csv',
  
  parsers: {
    csvvalidator: {
      exec: ['csv_validator', '<inputFile>'],
      version: '0.1',
      parse: {
        type: 'regex-counter',
        version: '1',
        stdout: {
          '(.*)': {
            nameGroup: 1,
          },
        },
      },
    },
  },
  decision_default: {file: 'dsl.txt'},
  decision_views: {
  },

  file_detail_views: {
  },
  build: {
    stages: {
      base: {
        from: 'ubuntu:22.04',
        commands: [
          'RUN apt-get update && apt-get install -y python3 python3-pip'
        ]
      },
      csvvalidator: {
        commands: [
          'COPY ./csv/csv_validator.py /tmp/csv_validator.py'
        ],
        copy_output: {
          '/tmp/csv_validator.py': '/usr/bin/csv_validator'
        },
      },
    },
  },
}
```

In the next few sections we will describe relevant portions of the configuration. For
detailed comments on what each section/field in the configuration means, please take
a look at the [configuration file](/pdf/config.json5) for the PDF distribution .

## Build Subsection

The `build` subsection of the configuration describes the installation process for the simple CSV validator, `csv_validator.py`, described below. The script is placed directly in the distribution folder.

```python
import sys
import csv

with open(sys.argv[1], newline='') as f:
  r = csv.reader(f)
  count = len(next(r))
  for row in r:
    assert len(row) == count
```

The build process involves two stages: in the base stage, we request that python be installed, which ensures that it will be available as part of the final docker image for the distribution; in the second stage, the script is added to the docker image using the `COPY` command. 

The `copy_output` parameter ensures it is placed in its designated location within the image. Although not essential for this straightforward configuration, this specification proves advantageous in cases where the build process is intricate but only a few build artifacts are needed for runtime execution.

## Parser Subsection

The `parsers` section contains a specification for a parser, based on the CSV validation script above. It defines the command line to execute the validator and the method to map the validator's output to parser features. In this particular case, all output lines are directly captured and stored as features.

The configuration for the PDF distribution contains additional examples of parser configurations that exercises more of the tooling around tool output to parser feature conversions.

## Default Decisions Subsection

This section uses FAW's decision DSL to define what a valid file would look like. In this case, the decision DSL is stored in a file named `dsl.txt` at the root of distribution folder. The file can potentially be empty to get started.

The PDF distribution provides a more complicated example of a decision DSL.
