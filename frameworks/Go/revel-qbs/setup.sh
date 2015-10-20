#!/bin/bash

sed -i 's|tcp(.*:3306)|tcp('"${DBHOST}"':3306)|g' src/benchmark/conf/app.conf

fw_depends go

go get -u github.com/robfig/revel/revel github.com/coocood/qbs
go build -o bin/revel github.com/robfig/revel/revel
bin/revel run benchmark prod &
