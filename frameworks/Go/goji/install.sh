#!/bin/bash


fw_depends go

export GOROOT=${IROOT}/go

export GOPATH=${TROOT}

# Where to find the go executable
export PATH="$GOROOT/bin:$PATH"

go get github.com/go-sql-driver/mysql
go get github.com/zenazn/goji
go get github.com/zenazn/goji/web
