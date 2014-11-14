#!/bin/bash

# Where to find the go executable
export PATH="$GOROOT/bin:$PATH"

go get -u github.com/robfig/revel/revel github.com/coocood/qbs
go build -o bin/revel github.com/robfig/revel/revel
bin/revel run benchmark prod &