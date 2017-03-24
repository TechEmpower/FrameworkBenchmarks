#!/bin/bash

sed -i 's|tcp(.*:3306)|tcp('"${DBHOST}"':3306)|g' src/hello-orm-mysql/models/init.go

fw_depends mysql go

curl https://glide.sh/get | sh
glide -v

cd src/hello-orm-mysql
glide install
go run main.go &
