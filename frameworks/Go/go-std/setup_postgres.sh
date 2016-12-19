#!/bin/bash

fw_depends postgres go

go get github.com/lib/pq

go run hello_postgres.go &
