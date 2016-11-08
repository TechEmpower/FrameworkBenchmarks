#!/bin/bash
#
# tear-down-environment.sh
#
# Tears down the benchmark environment in preparation for another
# run.
#
# @author A. Shawn Bandy
#
# $TFB_REPOPARENT parent folder of the repository
# $TFB_REPONAME   name of the repository folder
sudo rm -r -f $TFB_REPOPARENT/$TFB_REPONAME
cd $TFB_REPOPARENT
