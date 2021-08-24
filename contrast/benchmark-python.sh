#!/bin/bash
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
../tfb --tag contrast --test-lang Python --type fortune --duration 60

# Stop contrast-service container
docker stop contrast-service