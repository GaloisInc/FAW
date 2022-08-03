#!/usr/bin/env bash

# Run the command
ulimit -v 48000000
/parsec/pdf/pdf "$@"
code=$?

# Remove extra files that are being created
rm *psectxt &>/dev/null
rm *strtxt &>/dev/null

exit $code
