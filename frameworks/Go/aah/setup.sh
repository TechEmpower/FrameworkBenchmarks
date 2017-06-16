#!/bin/bash

fw_depends mysql go

set -x

mkdir bin

# Install aah CLI tool and glide
go get aahframework.org/tools.v0/aah
curl https://glide.sh/get | sh

# Clean src directory, glide is used to get dependencies libraries
rm -rf src/aahframework.org
rm -rf src/golang.org

# print version
aah version
glide -v

# Run glide to get dependencies
cd src/benchmark
glide install

# Build and unpack
aah build
cd build
unzip benchmark-0.0.1-linux-amd64.zip

# Start server
./benchmark/aah.sh start
