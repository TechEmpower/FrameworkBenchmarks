#!/bin/bash

sed -i 's|tcp(.*:3306)|tcp('"${DBHOST}"':3306)|g' src/kami/server.go

fw_depends go

go get github.com/go-sql-driver/mysql
go get github.com/guregu/kami

go run src/kami/server.go &
