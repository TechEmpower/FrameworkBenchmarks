#!/bin/bash
#
# A list of tasks to be run for continuous benchmarking
#

##########################
#  Tasks before the run  #
##########################

$TFB_REPOPARENT/$TFB_REPONAME/continuous/tasks/record-git-commit.sh


##########################
#   Run the benchmarks   #
##########################

$TFB_REPOPARENT/$TFB_REPONAME/continuous/tasks/run-benchmarks.sh

##########################
#  Tasks after the run   #
##########################

$TFB_REPOPARENT/$TFB_REPONAME/continuous/tasks/keep-logs.py
