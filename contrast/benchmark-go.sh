#!/bin/bash
source ./benchmark-variables.sh

set -e

cd "${0%/*}"

# Use files in the script directory by default
AGENT_FILE=${1-contrast-go}

if [[ $(basename -- "$AGENT_FILE") != "contrast-go" ]];
    then echo "First argument must be a path to contrast-go" && exit 1;
fi;

if [[ ! -f $AGENT_FILE ]];
    then echo "Could not find $AGENT_FILE" && exit 1;
fi;

# Copy files into correct place
./copy-files.sh Go $AGENT_FILE

# Start contrast-service
./start-contrast-service.sh

# Run tests
../tfb --tag $TAG --test-lang Go --type fortune --duration $DURATION --concurrency-levels $CONCURRENCY_LEVELS

# Stop contrast-service container
docker stop contrast-service
