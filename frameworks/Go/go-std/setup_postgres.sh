#!/bin/bash

fw_depends go

go get github.com/lib/pq

go run hello_postgres.go &
