#!/usr/bin/env bash

#CONTINUE_TEST=false
#GIT_TARGET=remote/origin/master
#
## Check to see if there are changes in the toolset
#toolChanges=$(git diff $GIT_TARGET toolset | wc -l)
#if [[ $toolChanges -ne 0 ]]; then
#    echo "Changes found in the toolset. Testing all frameworks."
#    CONTINUE_TEST=true
#fi
#
## Check to see if there are changes in the framework
#fwChanges=$(git diff $GIT_TARGET frameworks/$TESTDIR | wc -l)
#if [[ $fwChanges -ne 0 ]]; then
#    echo "Changes found in the framework, continuing to test."
#    CONTINUE_TEST=true
#fi
#
#if [[ $CONTINUE_TEST == false ]]; then
#    echo "No changes found for this framework. Skipping test."
#    exit 0
#fi

# Parse the benchmark_config for the names of all tests
sudo apt-get install jq
FW_TEST=`jq -r '.framework ' frameworks/${TESTDIR}/benchmark_config.json`
export TESTS=`jq '.tests[0] | keys | . - ["default"] | map("'${FW_TEST}-'"+.) | ["'${FW_TEST}'"] + . | .[] | tostring ' frameworks/${TESTDIR}/benchmark_config.json | xargs echo`
