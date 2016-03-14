#!/bin/bash

sed -i 's|localhost|'"${DBHOST}"'|g' server.go

fw_depends go

go get -u github.com/jackc/pgx
go get -u github.com/valyala/fasthttp
go get -u github.com/valyala/quicktemplate/...

rm -f ./server
go build -o server
./server &
