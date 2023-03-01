#! /usr/bin/env python3

import code
import multiprocessing.connection
import os
import pymongo
import readline
import rlcompleter
import sys

# Connect a pymongo instance with 'db' being the database corresponding to this
# instance.
_c = pymongo.MongoClient()
db = _c[os.environ['DB']]


def clear_database():
    # Hardcoded port corresponds to (8123 + 2);
    # If run in docker, the FAW server will listen on this port
    conn = multiprocessing.connection.Client(('localhost', 8125))
    print("Clearing DB and resetting file list...")
    conn.send('clear_database')
    conn.recv()  # Block until DB is cleared
    print("Cleared DB.")
    conn.close()


extra_vars = {
    'db': db,
    'clear_database': clear_database,
}

# Enable tab completion in the REPL
readline.set_completer(rlcompleter.Completer(extra_vars).complete)
readline.parse_and_bind("tab: complete")

banner = f"""
Python {sys.version} on {sys.platform}
Use the `db` variable to interact with the database.
Run `clear_database` to clear the database."""
code.interact(banner=banner, local=extra_vars)
