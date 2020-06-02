# Issues (Todo,Etc)
## max size of stdout and stderr
These text fields are now truncated to 1M bytes
  - "result.stdoutRes"
  - "result.stderrRes"

If we need to truncate, these fields will end with:

    "\nERROR: DATA-TRUNCATED\n"

This may cause parsing issues for the last truncated line, no effort is made
to truncate at a newline.

To quickly search for truncated fields in mongo shell:

    db.rawinvocations.find({"result.stdoutRes": /ERROR: DATA-TRUNCATED$/}).count();
    db.rawinvocations.find({"result.stderrRes": /ERROR: DATA-TRUNCATED$/}).count();
