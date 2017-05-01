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
  # Tear down the environment
  # Note: we tear down first so that we can know the state of the environment
  # regardless of the outcome of prior runs.
  if [ -d "$TFB_REPOPARENT/$TFB_REPONAME" ]; then
    cp $TFB_REPOPARENT/$TFB_REPONAME/benchmark.cfg $TFB_REPOPARENT/
    cp $TFB_REPOPARENT/$TFB_REPONAME/toolset/lifecycle/rebuild-environment.sh $TFB_REPOPARENT/
    echo Tearing down previous environment
    $TFB_REPOPARENT/$TFB_REPONAME/toolset/lifecycle/tear-down-environment.sh
  fi
  # Rebuild environment (i.e. clone the repository)
  echo Rebuilding environment
  $TFB_REPOPARENT/rebuild-environment.sh
  echo Returning benchmark configuration file
  cp $TFB_REPOPARENT/benchmark.cfg $TFB_REPOPARENT/$TFB_REPONAME/
  # Handle any preprocessing (e.g. send metadata)
  echo Running pre-test tasks
  for SCRIPT in $TFB_REPOPARENT/$TFB_REPONAME/toolset/lifecycle/pre-run-tests/*
    do
      if [ -f $SCRIPT -a -x $SCRIPT ]
      then
        $SCRIPT
      fi
    done
  sleep 5
  # Run the benchmarks
  echo Change to benchmark root
  cd $TFB_REPOPARENT/$TFB_REPONAME
  echo Run tests
  toolset/run-tests.py
  # Handle any postprocessing
  echo Running post-test tasks
  for SCRIPT in $TFB_REPOPARENT/$TFB_REPONAME/toolset/lifecycle/post-run-tests/*
    do
      if [ -f $SCRIPT -a -x $SCRIPT ]
      then
        $SCRIPT
      fi
    done
  sleep 5
done
