#!/bin/bash

# Defaults
ds=/var/run/docker.sock
sd=`pwd`

# Build the tfb image
docker pull techempower/tfb

# Create the tfb network
docker network create tfb > /dev/null 2>&1
# Run the suite
docker run --network=tfb -v ${DOCKER_SOCKET_PATH-$ds}:/var/run/docker.sock --mount type=bind,source=${TFB_SOURCE_DIR-$sd},target=/FrameworkBenchmarks techempower/tfb "$@"
