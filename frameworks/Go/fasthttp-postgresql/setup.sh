#!/bin/bash

sed -i 's|tcp(.*:3306)|tcp('"${DBHOST}"':3306)|g' src/hello/hello.go

fw_depends go

go get -u github.com/jackc/pgx
go get -u github.com/valyala/fasthttp

go run src/hello/hello.go &
