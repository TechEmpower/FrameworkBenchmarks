#!/bin/bash

fw_depends go

go get github.com/hoisie/web

go run src/hello/hello.go &
