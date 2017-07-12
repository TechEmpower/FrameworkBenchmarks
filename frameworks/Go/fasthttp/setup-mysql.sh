#!/bin/bash

sed -i 's|tcp(.*:3306)|tcp('"${DBHOST}"':3306)|g' src/server-mysql/server.go

fw_depends mysql go

GOPATH=`pwd`

rm -rf ./pkg/*
go get -d -u github.com/go-sql-driver/mysql
go get -d -u github.com/valyala/fasthttp/...
go get -u github.com/valyala/quicktemplate/qtc

rm -f ./server-mysql
go generate templates
go build -gcflags='-l=4' server-mysql

./server-mysql &
