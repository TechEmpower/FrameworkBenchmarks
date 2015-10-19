#!/bin/bash

sed -i 's|tcp(.*:3306)|tcp('"${DBHOST}"':3306)|g' src/hello/hello.go

fw_depends go

go get github.com/astaxie/beego
go get github.com/go-sql-driver/mysql

go run src/hello/hello.go &
