#!/bin/bash
#
# A list of tasks to be run for continuous benchmarking
#

##########################
#  Tasks before the run  #
##########################

$TFB_REPOPARENT/$TFB_REPONAME/toolset/continuous/tasks/record-git-commit.sh


##########################
#   Run the benchmarks   #
##########################

$TFB_REPOPARENT/$TFB_REPONAME/toolset/continuous/tasks/run-benchmarks.sh

##########################
#  Tasks after the run   #
##########################

FWROOT=$TFB_REPOPARENT/$TFB_REPONAME PYTHONPATH=$TFB_REPOPARENT/$TFB_REPONAME python $TFB_REPOPARENT/$TFB_REPONAME/toolset/continuous/tasks/keep-logs.py
