#!/bin/bash

sed -i 's|tcp(.*:3306)|tcp('"${DBHOST}"':3306)|g' src/server-mysql/server.go

fw_depends go

GOPATH=`pwd` go get -u github.com/go-sql-driver/mysql
GOPATH=`pwd` go get -u github.com/valyala/fasthttp/...
GOPATH=`pwd` go get -u github.com/valyala/quicktemplate/qtc

rm -f ./server-mysql
GOPATH=`pwd` go generate templates
GOPATH=`pwd` go build server-mysql
./server-mysql &
