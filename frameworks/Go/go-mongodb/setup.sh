#!/bin/bash

# Set the root of our go installation
export GOROOT=${IROOT}/go
export GOPATH=${TROOT}

# Where to find the go executable
export PATH="$GOROOT/bin:$PATH"

go get ./...

go run src/hello/hello.go &