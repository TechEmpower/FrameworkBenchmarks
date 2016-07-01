#!/bin/bash

sed -i 's|connectionString = "localhost"|connectionString = "'"${DBHOST}"'"|g' hello_mongo.go

fw_depends go

go get gopkg.in/mgo.v2

go run hello_mongo.go &
