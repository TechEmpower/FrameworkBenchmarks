#!/bin/bash
#
# Start the benchmarks

echo Change to benchmark root
cd $TFB_REPOPARENT/$TFB_REPONAME
echo Running tests
PYTHONPATH=$TFB_REPOPARENT/$TFB_REPONAME python toolset/run-tests.py