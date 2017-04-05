#!/bin/bash

fw_depends go

curl https://glide.sh/get | sh
glide -v

cd src/hello
glide install
go run main.go &
