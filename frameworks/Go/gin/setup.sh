#!/bin/bash

fw_depends go
export GOPATH=${TROOT}

go get github.com/gin-gonic/gin
go get github.com/go-sql-driver/mysql

go run hello.go &
