#! /usr/bin/env python3

import code
import os
import pymongo

# Connect a pymongo instance with 'db' being the database corresponding to this
# instance.
_c = pymongo.MongoClient()
db = _c[os.environ['DB']]

print(f'Use the `db` variable to interact with the database')
code.interact(local=dict(globals(), **locals()))

