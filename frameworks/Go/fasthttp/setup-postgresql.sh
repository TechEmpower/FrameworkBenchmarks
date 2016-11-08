#!/bin/bash

sed -i 's|localhost|'"${DBHOST}"'|g' src/server-postgresql/server.go

fw_depends go

GOPATH=`pwd` go get -u github.com/jackc/pgx
GOPATH=`pwd` go get -u github.com/valyala/fasthttp/...
GOPATH=`pwd` go get -u github.com/valyala/quicktemplate/qtc

rm -f ./server-postgresql
GOPATH=`pwd` go generate templates
GOPATH=`pwd` go build server-postgresql
./server-postgresql &
