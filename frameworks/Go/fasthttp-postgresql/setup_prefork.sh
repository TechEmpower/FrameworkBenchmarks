#!/bin/bash

sed -i 's|localhost|'"${DBHOST}"'|g' server.go

fw_depends go

go get -u github.com/jackc/pgx
go get -u github.com/valyala/fasthttp
go get -u github.com/valyala/quicktemplate/qtc

rm -f ./server
go generate
go build -o server
./server -prefork &
