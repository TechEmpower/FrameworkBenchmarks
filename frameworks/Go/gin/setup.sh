#!/bin/bash

fw_depends go

go get github.com/gin-gonic/gin
go get github.com/go-sql-driver/mysql

go run hello.go &
