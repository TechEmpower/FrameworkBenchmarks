#!/bin/bash

# Where to find the go executable
export PATH="$GOROOT/bin:$PATH"

go get ./...

go run src/hello/hello.go &