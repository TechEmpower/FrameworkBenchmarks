#!/bin/bash

export GOROOT=${IROOT}/go

export GOPATH=${TROOT}

# Where to find the go executable
export PATH="$GOROOT/bin:$PATH"

go run src/goji/server.go &
