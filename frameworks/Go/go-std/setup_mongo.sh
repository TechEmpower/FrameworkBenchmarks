#!/bin/bash

sed -i 's|connectionString = "localhost"|connectionString = "'"${DBHOST}"'"|g' hello_mongo.go

fw_depends mongodb go libsasl2-dev 

go get gopkg.in/mgo.v2

go run hello_mongo.go &
