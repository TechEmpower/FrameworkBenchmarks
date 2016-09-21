#!/bin/bash
#
# rebuild-environment.sh
#
# Rebuilds the benchmarking environment.
#
cd $TFB_REPOPARENT
git clone -b $TFB_REPOBRANCH $TFB_REPOURI
cd $TFB_REPOPARENT/$TFB_REPONAME

#
# Make the results directory if it doesn't exist.
#
# save the current git commit to results/.commit
#
mkdir -p results
git rev-parse HEAD > results/.commit
