#!/bin/bash

fw_depends go

sed -i 's|tcp(.*:3306)|tcp('"${DBHOST}"':3306)|g' src/benchmark/conf/app.conf

go get -u github.com/revel/cmd/revel
revel=$GOPATH/bin/revel
GOPATH=$GOPATH:`pwd` $revel run benchmark prod &
