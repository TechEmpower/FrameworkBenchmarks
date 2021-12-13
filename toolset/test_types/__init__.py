import imp
import re

from glob import glob

test_types = {}
test_type_folders = glob("/FrameworkBenchmarks/toolset/test_types/*/")

# Loads all the test_types from the folders in this directory
for folder in test_type_folders:
    # regex that grabs the characters between "toolset/test_types/"
    # and the final "/" in the folder string to get the name
    test_type_name = re.findall(r'.+\/(.+)\/$', folder, re.M)[0]
    test_type = imp.load_source("TestType", "%s%s.py" % (folder, test_type_name))
    test_types[test_type_name] = test_type.TestType
