#!/bin/bash

fw_depends go

go get github.com/go-sql-driver/mysql

go run hello_mysql.go -prefork &
