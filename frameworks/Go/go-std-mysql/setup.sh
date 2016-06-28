#!/bin/bash

fw_depends go

go get ./...

go run src/hello/hello.go &
