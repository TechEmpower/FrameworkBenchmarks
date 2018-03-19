#!/bin/bash
#
# Start the benchmarks

echo Change to benchmark root
cd $TFB_REPOPARENT/$TFB_REPONAME
echo Cleaning
FWROOT=$TFB_REPOPARENT/$TFB_REPONAME PYTHONPATH=$TFB_REPOPARENT/$TFB_REPONAME python toolset/run-tests.py --clean
echo Running tests
FWROOT=$TFB_REPOPARENT/$TFB_REPONAME PYTHONPATH=$TFB_REPOPARENT/$TFB_REPONAME python toolset/run-tests.py