#!/bin/bash

fw_depends postgresql go

set -x

# Install glide into GOPATH
mkdir -p bin
curl https://glide.sh/get | sh
glide -v

pushd src
glide install
popd

go install app

app &
