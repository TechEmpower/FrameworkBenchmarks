#!/bin/bash

# Where to find the go executable
export PATH="$GOROOT/bin:$PATH"


go get github.com/astaxie/beego
go get github.com/go-sql-driver/mysql

go run src/hello/hello.go &