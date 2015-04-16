#!/bin/bash

fw_depends go
export GOPATH=${TROOT}

go get github.com/hoisie/web

go run src/hello/hello.go &
