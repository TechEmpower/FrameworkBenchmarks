import importlib
import re

from colorama import Fore
from glob import glob
from toolset.utils.output_helper import log

databases = {}
db_folders = glob("/FrameworkBenchmarks/toolset/databases/*/")

# Loads all the databases from the folders in this directory
# and checks to see if they've implemented the required methods
for folder in db_folders:
    # regex that grabs the characters between "toolset/database/"
    # and the final "/" in the db folder string to get the db name
    db_name = re.findall(r'.+\/(.+)\/$', folder, re.M)[0]
    # ignore generated __pycache__ folder
    if db_name == '__pycache__':
        continue
    spec = importlib.util.spec_from_file_location("Database", "%s%s.py" % (folder, db_name))
    db = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(db)

    if not hasattr(db.Database, "get_current_world_table")\
            or not hasattr(db.Database, "test_connection"):
        log("Database %s does not implement the required methods" % (db_name),
            color=Fore.RED)

    databases[db_name] = db.Database
