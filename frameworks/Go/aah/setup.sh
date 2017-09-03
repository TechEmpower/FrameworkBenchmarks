#!/bin/bash

fw_depends mysql go

set -x

mkdir -p bin

# Install aah CLI tool (installing specific tag version)
mkdir -p src/aahframework.org
git clone https://github.com/go-aah/tools.git src/aahframework.org/tools.v0
cd src/aahframework.org/tools.v0
git checkout tags/v0.8 -b v0.8
cd -
go get aahframework.org/tools.v0/aah/...
go install aahframework.org/tools.v0/aah

# print aah version
aah -v

# Install glide
curl https://glide.sh/get | sh
glide -v

# Clean src directory, glide is used to get dependencies libraries
rm -rf src/aahframework.org src/golang.org src/gopkg.in

# Run glide to get dependencies
cd src/benchmark
glide install

# Build and unpack
aah build -o build/benchmark.zip
cd build
unzip benchmark.zip

# Start server
./benchmark/aah.sh start
