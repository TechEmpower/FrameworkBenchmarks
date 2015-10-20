#!/bin/bash

sed -i 's|tcp(.*:3306)|tcp('"${DBHOST}"':3306)|g' src/goji/server.go

fw_depends go

go get github.com/go-sql-driver/mysql
go get github.com/zenazn/goji
go get github.com/zenazn/goji/web

go run src/goji/server.go &
