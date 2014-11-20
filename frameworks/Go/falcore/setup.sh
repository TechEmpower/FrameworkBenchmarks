#!/bin/bash


#!/bin/bash

# Where to find the go executable
export PATH="$GOROOT/bin:$PATH"

go get ./...
go run src/framework_benchmarks/falcore.go &