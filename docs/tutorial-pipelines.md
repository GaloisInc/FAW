# Pipelines in the FAW

A lot of the FAW documentation for pipelines lives in the config file attached to the demonstration plugin in [`pdf/ml_test/config.json5`](../pdf/ml_test/config.json5).  This tutorial contains a slightly more verbose breakdown of the architecture through which the FAW supports computational pipelines for more involved data processing.

In general, bear in mind that what pipelines provide is a means of managing long-running tasks, which produce artifacts used by parsers or other plugins.

## Making a new pipeline plugin

It is recommended to begin by copying (e.g., `cp -a`) the `ml_test` plugin from the `pdf` distribution, and renaming it. The below information should help with adapting that plugin to whatever purpose is desired.

Note that any software requirements may be added to the docker container through the `build` key in `config.json5`. Anything not under the `final` stage will be added to the final container; see [the pdf distribution config file](../pdf/config.json5) for examples of stages other than `final` which build e.g. Pdfium, and copy the result of the build to the final image.

## Pipeline overview

Knowledge of the general FAW architecture, including the interrelation between the config options `parsers`, `file_detail_views`, and `decision_views` is assumed. For additional information on those, [see the main config file](../pdf/config.json5).

Pipelines add an additional dimension: `tasks`. Tasks denote processing chunks which are allowed to iterate through all files available to the FAW, potentially multiple times, and can store the result of those computations into dedicated collections in the MongoDB instance managed by the FAW. Pipelines can also add new `parsers`, `file_detail_views`, or `decision_views`, which are merged with the top-level keys of matching name. Importantly, when these plugins are specified as part of a pipeline, they may pass the `<apiInfo>` argument to the called program, which contains the information needed to pull back out artifacts which were stored as part of the task.

As denoted in `config.json5`, FAW tasks may have a `dependsOn` member to specify an order of processing. Any time a `version` field is bumped for a task, that task's collections will be deleted from MongoDB, resulting in it and all of its dependents being reprocessed.

Once all tasks belonging to a pipeline have finished processing, any `parsers` defined for the pipeline will be reset and run on all files (that is, using the latest results from all tasks' processing).

As for any `file_detail_views` or `decision_views`, the user may execute those at any time. The FAW UI shows the progress of tasks, and so it is the user's responsibility to run them _after_ tasks are complete.

For a demonstration of all of these concepts, view [the demonstration `ml_test` plugin](../pdf/ml_test), beginning with `config.json5`, and following the `exec` keys, following the flow of pipeline processing (tasks -> parsers).


## Runtime environment

Pipeline plugins may be anything executable, and run within the docker container running the FAW. The working directory is the folder containing the relevant `config.json5`. Other environment variables may be specified in the config, but remember that the runtime environment is **not** necessarily what would be expected outside of the FAW. This particularly applies when using Dask functionality.

In `--development` mode, be sure to touch any `config.json5` file that is part of the deployment to live-reload code. This in particular applies to pipeline functionality which uses dask, as restarting the dask processes is necessary (and handled by the FAW when a `config.json5` file is touched).


## `<apiInfo>`

This `<apiInfo>` key can be accessed in Python as follows:

```python
import faw_pipelines_util  # Module accessible within the FAW
import json

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('api_info', type=json.loads)
    args = ap.parse_args()

    api = faw_pipelines_util.Api(args.api_info)

if __name__ == '__main__':
    main()
```

Notably, once the `api` object is available, it has a number of useful functions.

### File

The `api` object has a number of `file_` methods which allow access to the files being investigated by the FAW.

`api.file_count` returns the number of total files being investigated by the FAW. It may be used to e.g. determine how many iterations of an ML algorithm should be run.

`api.file_fetch` provides a context manager (e.g., for use with a `with` block), which transforms a path from the FAW's internals into a local, on-disk path. See [cmd_train.py](../pdf/ml_test/cmd_train.py#L79), the `ExitStack` bit, for how this might be used to fetch a batch of files.

`api.file_list` provides a way of getting a single python list with all FAW-internal paths of available files. These must be passed through `api.file_fetch` to be of use. Note: this function is **only suitable for small FAW deployments**! It will not work on larger ones, as the list may be prohibitively large.

`api.file_sample` provides a means of taking a random sample of `n` files from the data set. **This function is suitable for arbitrarily large FAW distributions.**

### Mongo

`api.mongo_collection` provides a means of getting a, e.g. `pymongo`, instance pointing to a collection within this specific task's storage. If used outside of a FAW `task`, the `taskname` parameter must be passed, since this method only returns references to collections tied to the task.

### File-like interface

These methods are provided to save and load data which the task generates. Notably, when used in any of the non-task plugins (e.g., `parsers` etc.), the `taskname` parameter should be passed to specify which task's data is being accessed.

`task_file_write(name[, taskname=?])` returns a file-like object which should be written to and closed. The recommended way of doing this is a `with` block, similar to the standard `open()` function in python's standard library.

`task_file_read(name[, taskname=?])` returns a file-like object which can be read. The recommended way of doing this is a `with` block. If passing this result directly to a file loader like `torch.load`, which uses `seek`, we recommend reading the file into a `io.BytesIO` object, and passing that instead. The `seek` method is very, very slow when it has to reach out to MongoDB.

### Task status interface

`api.task_status_set_message('message')` will update the status message associated with the current task which is shown in the FAW UI.  This is purely used for debugging / progress reporting.

### Dask

On a single machine, as is often the case when developing with the FAW, dask does not provide much benefit. However, with a cluster, dask provides a way of accessing the additional compute power of multiple machines.

The `api.dask_get_client()` returns a [Dask client](https://distributed.dask.org/en/latest/client.html) object which may be used as expected. The easiest way to work with dask is through the [delayed API](https://docs.dask.org/en/latest/delayed.html). However, for operations where loading data is expensive, the [actor API](https://distributed.dask.org/en/latest/actors.html) may help; the actor approach [may be seen in `pdf/ml_test/dask_util.py`](../pdf/ml_test/dask_util.py).

