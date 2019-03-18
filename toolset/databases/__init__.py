import imp
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
    db = imp.load_source("Database", "%s%s.py" % (folder, db_name))

    if not hasattr(db.Database, "get_current_world_table")\
            or not hasattr(db.Database, "test_connection"):
        log("Database %s does not implement the required methods" + db_name,
            color=Fore.RED)

    databases[db_name] = db.Database
