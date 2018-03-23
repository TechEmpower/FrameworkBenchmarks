#!/bin/bash

docker run --network=host -v //var/run/docker.sock:/var/run/docker.sock --mount type=bind,source=c:/Development/FrameworkBenchmarks,target=/FrameworkBenchmarks tfb "$@"
