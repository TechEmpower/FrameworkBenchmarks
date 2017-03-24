#!/bin/bash

sed -i 's|tcp(.*:3306)|tcp('"${DBHOST}"':3306)|g' src/hello/models/init.go

fw_depends mysql go

curl https://glide.sh/get | sh
glide -v

cd src/hello
glide install
go run main.go &
