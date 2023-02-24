#! /usr/bin/env python3

import code
import os
import pymongo
import readline
import rlcompleter

# Connect a pymongo instance with 'db' being the database corresponding to this
# instance.
_c = pymongo.MongoClient()
db = _c[os.environ['DB']]

# Enable tab completion in the REPL
vars = {**globals(), **locals()}
readline.set_completer(rlcompleter.Completer(vars).complete)
readline.parse_and_bind("tab: complete")

print(f'Use the `db` variable to interact with the database')
code.interact(local=vars)
