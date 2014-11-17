#!/bin/bash

# here te find the go executable
export PATH="$GOROOT/bin:$PATH"

go get -u github.com/robfig/revel/revel github.com/eaigner/jet
go build -o bin/revel github.com/robfig/revel/revel
bin/revel run benchmark prod &