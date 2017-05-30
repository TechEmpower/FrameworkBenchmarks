#!/bin/bash
#
# Sets up and runs the benchmarking suite in an infinite loop.  The intent is 
# this script would be run by a service such as upstart, restarting if the 
# scripts fail.
#
# The following environment variables must
# be set:
# $TFB_REPOPARENT   parent folder of the repository
# $TFB_REPONAME     name of the repository folder
# $TFB_REPOURI      URI for the git repository
# $TFB_MAILINGLIST  email address for the mailing list
# $TFB_LOGSFOLDER   location to transfer logged output
#
# @author A. Shawn Bandy
#

while true
do
  # This file is copied outside the repo and run by tfb service
  # so that it may tear down the environment and run without interruption.
  # If this file is changed, the tfb service will need to be restarted.

  # Now if tasks are updated in the continuous toolset, they will execute
  # properly on the next full run.

  # Start by tearing down the environment.
  # Hold on to the benchmark.cfg it it exists
  echo Tearing down and rebuilding the environment
  cp $TFB_REPOPARENT/$TFB_REPONAME/benchmark.cfg $TFB_REPOPARENT/benchmark.cfg 2>/dev/null
  sudo rm -rf $TFB_REPOPARENT/$TFB_REPONAME
  cd $TFB_REPOPARENT
  git clone -b $TFB_REPOBRANCH $TFB_REPOURI $TFB_REPOPARENT/$TFB_REPONAME
  # Replace the benchmark.cfg
  cp $TFB_REPOPARENT/benchmark.cfg $TFB_REPOPARENT/$TFB_REPONAME/benchmark.cfg 2>/dev/null

  echo Running continuous tasks
  $TFB_REPOPARENT/$TFB_REPONAME/toolset/continuous/tasks/run-tasks.sh

  sleep 5
done
