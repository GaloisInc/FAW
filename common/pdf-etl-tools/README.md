# pdf-etl-tools

A set of tools, written in Haskell, for invoking various pdf tools
as well as populating and querying a [local] mongo db.

## Caveat

Currently, `pdf-etl-tool retry` will not work if you have uniqueness keys
on the collection!

## FIXME's

 1. fix the above CAVEAT.
 2. <much more>

## Building

1. Install stack, see https://docs.haskellstack.org/.

2. In this directory, do

        stack build

   which will download the proper versions of ghc and required packages, and
   build the libraries and tool.

You can just leave the binary inside the stack internal build directories and
execute the pdf-etl-tool using stack like this


    stack run pdf-etl-tool -- arg1 arg2 arg3 ...


## Help

    stack run pdf-etl-tool -- --help           # general help
    stack run pdf-etl-tool -- add-raw --help   # help for the add-raw command

## Test Run

Usage:

    stack run pdf-etl-tool -- \
       -v -d littlegovdocs -c rawinvocations add-raw -ipdfinfo,qpdf-check ../test/data/*.pdf

This will
  - for each pdf in `../test/data/*.pdf`
    - run two tools (AKA "invokers") on the pdf ("pdfinfo" and "qpdf-check")
    - capture exit status, stdout, stdin, the runtime
    - connect to a local mongo db and write the above information to
      - database "littlegovdocs"
      - collection "rawinvocations"

## Configuration Files for Specifying Tools (Invokers):

See the `invokers.cfg` file in this directory *and the comments.* pdf-etl-tool
expects to find a file with this name in the current directory, but you can tell
`pdf-etl-tool` where to find the file thus:

    --invokersfile <path-to-your-invokers.cfg>

The invokers are indexed by a unique string, e.g.,

    invName = "pdfinfo"

This is the name you want to use on the command line after the `--invokers` (`-i`) option.

## Timeouts

Each invoker in the `invokers.cfg` file specifies a timeout "factor", however
you can override this (overriding for all the invokers designated) by using one
of these `add-raw` command options:

     --timeout SECONDS
     --timeout no-timeout

Note that the *scale* in `invokers.cfg` is *microsecs per byte* but here we have
*seconds*.  If you leave this flag out, or specify `--timeout no-override`,
`add-raw` will use the timescale associated with the invoker in `invokers.cfg`.

## Typical Usage

Something like this is likely how other tools will invoke `pdf-etl-tool`:

    stack run pdf-etl-tool -- \
      -d govdocs -c rawinvocations \
      add-raw \
      --timeout no-timeout \
      --invokers pdfinfo,pdfinfo-meta,pdfinfo-struct,pdftocairo-pdf,qpdf-check,pdfid \
      [YOUR-PDF-FILES-HERE]+

And if you really want to invoke every single invoker in the config file, you
can replace the 5th line with

      --invokers ALL \

## Re-entrancy

pdf-etl-tool is now "re-entrant": you can run multiple versions of it on the
same host.  However, if two instances are trying to update the database with
the same invocation (neither efficient, nor recommended) we're relying on
the transaction oriented nature of mongo to keep the database sane.

## Generating Raw Data from govdocs

If you dare pushing the limits of filename expansion one might:

     stack run pdf-etl-tool -- -v -d govdocs -c rawinvocations \
       add-raw -ipdfinfo /media/data/raw/govdocs/*.pdf

Or, to just process the first 1000 files (using zsh) you could

     pr -l /media/data/raw/govdocs/*.pdf | head -1000 | \
     stack run pdf-etl-tool -- -v -d govdocs -c rawinvocations \
     add-raw -ipdfinfo
