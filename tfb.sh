#!/bin/bash

#docker run --network=host -v //var/run/docker.sock:/var/run/docker.sock --mount type=bind,source=c:/Development/FrameworkBenchmarks,target=/FrameworkBenchmarks tfb "$@"

# Create the tfb network
docker network create tfb > /dev/null 2>&1
# Run the suite
docker run --network=tfb -v //var/run/docker.sock:/var/run/docker.sock --mount type=bind,source=c:/Development/FrameworkBenchmarks,target=/FrameworkBenchmarks tfb "$@"
