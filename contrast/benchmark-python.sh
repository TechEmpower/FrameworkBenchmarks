#!/bin/bash
source ./benchmark-variables.sh

set -e

cd "${0%/*}"

# Use files in the script directory by default
AGENT_FILE=${1-contrast-agent.tar.gz}

if [[ $(basename -- "$AGENT_FILE") != "contrast-agent.tar.gz" ]];
    then echo "First argument must be a path to contrast-agent.tar.gz" && exit 1;
fi;

if [[ ! -f $AGENT_FILE ]];
    then echo "Could not find $AGENT_FILE" && exit 1;
fi;

# Copy files into correct place
./copy-files.sh Python $AGENT_FILE

# Start contrast-service
./start-contrast-service.sh

# Run tests
../tfb --tag $TAG --test-lang Python --type fortune --duration $DURATION --concurrency-levels $CONCURRENCY_LEVELS

# Stop contrast-service container
docker stop contrast-service
