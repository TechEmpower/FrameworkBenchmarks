#!/bin/bash

sed -i 's|connectionString = "localhost"|connectionString = "'"${DBHOST}"'"|g' src/hello/hello.go

fw_depends go
export GOPATH=${TROOT}

go get ./...

go run src/hello/hello.go &
