#!/bin/bash

# Where to find the go executable
export PATH="$GOROOT/bin:$PATH"

go get -u github.com/robfig/revel/revel
go build -o bin/revel github.com/robfig/revel/revel
bin/revel run benchmark prod &