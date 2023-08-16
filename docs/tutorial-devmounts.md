# Introduction

FAW provides the infrastructure for developers to configure, build and run parser plugins from source. The "dev mount" feature builds on this to provide support for live development of parser plugins using a locally running FAW instance. In essence, dev mounts allow a plugin developer to "mount" a local (but external to the FAW) source repository/directory and configure parser plugins to build the parser based on this repository. Further, changes to this external directory are reflected within the running FAW instance and can automatically trigger a rebuild of the parser followed by a reparsing of the corpus with the rebuilt parser.

In this document, we introduce the feature using examples, most of which revolve around the pre-existing daedalus plugin in the FAW repository.


# Declaring a Dev Mount

A dev mount is declared in terms of an environment variable. A dev mount is considered *active* within a specific FAW instance if the environment variable has been defined (prior to running the instance) and points to a valid local directory. In the absence of the environment variable, the dev mount is considered *inactive* and related commands are treated as no-ops by the FAW. This mechanism is intended to allow plugin developers to maintain a single configuration file for both production and local development use cases, while being able to quickly move between the two scenarios as necessary.

Dev mounts are declared as part of the `build` section in a new `devmounts` subsection. Each dev mount declaration can also specify a collection of *triggers*, essentially a list of glob patterns; at runtime, updates to any file matching the specfied patterns are considered to be updates to the dev mount. (Updates to the dev mount, in turn, will prompt the FAW to rebuild stages and rerun parsers as configured; the details are provided in the sections below).

Here is an example of a dev mount configuration for the daedalus parser plugin:

    build: {
      devmounts: {
        ENV_DEVMOUNT_DAEDALUS: { 
          triggers: [ '**/pdf-hs-driver', '**/pdf-dom' ]
        }     
      }
      ...
    }

Here a pattern like `**/pdf-hs-driver` matches againt all files named `pdf-hs-driver` in any subdirectory under the dev mount directory. Note that pattern matching is done à la the Unix shell. See the [Python glob module](https://docs.python.org/3/library/glob.html) for a refresher on the specific details.


# Using the Dev Mount

To support dev mounts, a new FAW-specific command `FAW_DEVMOUNT <dev mount env variable> <target location>` has been introduced. The command can be part of any stage's build commands. If the specified dev mount is active, the command copies the external directory (tree) pointed to by the environment variable over to the specified target location; further it marks the stage as being associated with the dev mount in question and will trigger a rebuild of the stage when the dev mount is updated. It is worth noting that the copying process respects `.gitignore`, if present.

Here is an example of using a dev mount within the daedalus parser plugin:

    build: {
      stages: {
        pdf_hs_driver: {
          ...
          commands: [
            'WORKDIR /home',
            'RUN git clone https://github.com/GaloisInc/DaeDaLus daedalus \
              && cd daedalus \
              && git checkout 21e7f70510d1413641d764258a4187dd7b343ccc',
            //Newly added line: Dev mount the external source directory at the same location 
            'FAW_DEVMOUNT ENV_DEVMOUNT_DAEDALUS /home/daedalus',
            'WORKDIR /home/daedalus',
              ...
          ]
          ...
        }
        ...
      }
      ...
    }  

The only additional command added to the vanilla plugin configuration is the `FAW_DEVMOUNT` command. Its usage follows a standard pattern: the command is placed *after* `git clone` and the target location is set to that of `git clone`. When the dev mount is active, the command will cause the source to be overwritten with the contents of the external directory, while it is left intact if the dev mount is inactive. This pattern allows the rest of the build commands to remain consistent irrespective of whether the plugin is being used in a production or development environment.


# Configuring Parsers

Automatically rerunning *parsers* (for example, rerunning of daedalus-based PDF parser in our running example) on dev mount updates require additional configuration: a new key `devmounts` can be added as part of a parser's configuration which list the dev mounts that the parser depends on (if any). When such an entry is present, the parser is rerun when any of the specified dev mounts are updated; the reparsing of files occur after the associated stages have been rebuilt as described in the previous section.

Here is an example configuration for associating the daedalus PDF parser with a dev mount.

    parsers: {
      'pdf-hs-driver': {
        ...  
        devmount_dependencies: [ 'ENV_DEVMOUNT_DAEDALUS' ],
        ...
      }
      ...
    }

