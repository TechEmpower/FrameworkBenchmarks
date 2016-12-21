#!/bin/bash

fw_depends postgresql go

go get github.com/lib/pq

go run hello_postgres.go &
