#!/bin/bash

# sed -i 's|tcp(.*:3306)|tcp('"${DBHOST}"':3306)|g' src/hello/hello.go

# Where to find the go executable
export PATH="$GOROOT/bin:$PATH"

go get github.com/go-sql-driver/mysql
go get github.com/zenazn/goji
go get github.com/zenazn/goji/web

go run src/goji/server.go &