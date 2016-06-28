#!/bin/bash

sed -i 's|connectionString = "localhost"|connectionString = "'"${DBHOST}"'"|g' src/hello/hello.go

fw_depends go

go get gopkg.in/mgo.v2
go get ./...

go run src/hello/hello.go &
