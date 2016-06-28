#!/bin/bash

sed -i 's|tcp(.*:3306)|tcp('"${DBHOST}"':3306)|g' src/framework_benchmarks/falcore.go

fw_depends go

go get ./...
go run src/framework_benchmarks/falcore.go &
