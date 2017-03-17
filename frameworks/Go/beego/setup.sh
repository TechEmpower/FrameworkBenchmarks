#!/bin/bash

sed -i 's|tcp(.*:3306)|tcp('"${DBHOST}"':3306)|g' src/hello/models/init.go

fw_depends mysql go

go get -u github.com/astaxie/beego
go get -u github.com/go-sql-driver/mysql

cd src/hello
go run main.go &
