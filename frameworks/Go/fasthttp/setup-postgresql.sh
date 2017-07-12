#!/bin/bash

sed -i 's|localhost|'"${DBHOST}"'|g' src/server-postgresql/server.go

fw_depends postgresql go

GOPATH=`pwd`

rm -rf ./pkg/*
go get -d -u github.com/jackc/pgx
go get -d -u github.com/valyala/fasthttp/...
go get -u github.com/valyala/quicktemplate/qtc

rm -f ./server-postgresql
go generate templates
go build -gcflags='-l=4' server-postgresql

./server-postgresql &
