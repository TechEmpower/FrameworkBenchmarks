#!/bin/bash

# Where to find the go executable
export PATH="$GOROOT/bin:$PATH"

go get github.com/hoisie/web

go run src/hello/hello.go &