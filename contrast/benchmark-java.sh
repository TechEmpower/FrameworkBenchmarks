#!/bin/bash
source ./benchmark-variables.sh

set -e

cd "${0%/*}"

# Use files in the script directory by default
AGENT_FILE=${1-contrast.jar}

if [[ $(basename -- "$AGENT_FILE") != "contrast.jar" ]];
    then echo "First argument must be a path to contrast.jar" && exit 1;
fi;

if [[ ! -f $AGENT_FILE ]];
    then echo "Could not find $AGENT_FILE" && exit 1;
fi;

# Copy files into correct place
./copy-files.sh Java $AGENT_FILE

../tfb --tag $TAG --test-lang Java --type fortune --duration $DURATION --concurrency-levels $CONCURRENCY_LEVELS
