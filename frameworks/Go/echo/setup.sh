#!/bin/bash

sed -i 's|tcp(.*:3306)|tcp('"${DBHOST}"':3306)|g' server.go

fw_depends go

go get github.com/go-sql-driver/mysql
go get github.com/labstack/echo

go run server.go &
