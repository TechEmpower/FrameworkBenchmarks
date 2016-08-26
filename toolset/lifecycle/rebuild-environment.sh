#!/bin/bash
#
# rebuild-environment.sh
#
# Rebuilds the benchmarking environment.
#
cd $TFB_REPOPARENT
git clone -b $TFB_REPOBRANCH $TFB_REPOURI
cd $TFB_REPOPARENT/$TFB_REPONAME
