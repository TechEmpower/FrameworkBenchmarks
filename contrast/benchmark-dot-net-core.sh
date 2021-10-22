#!/bin/bash
source ./benchmark-variables.sh

set -e

cd "${0%/*}"

# Use files in the script directory by default
AGENT_FILE=${1-Contrast.NET.Core.zip}

if [[ $(basename -- "$AGENT_FILE") != "Contrast.NET.Core.zip" ]];
    then echo "First argument must be a path to Contrast.NET.Core.zip" && exit 1;
fi;

if [[ ! -f $AGENT_FILE ]];
    then echo "Could not find $AGENT_FILE" && exit 1;
fi;

# Copy files into correct place
./copy-files.sh CSharp $AGENT_FILE
./copy-files.sh VB $AGENT_FILE

../tfb --tag $TAG --test-lang CSharp VB --type fortune --duration $DURATION --concurrency-levels $CONCURRENCY_LEVELS
