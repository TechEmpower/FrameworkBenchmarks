#!/bin/bash

fw_depends go

go get ./...
go install ./...

fasthttp -prefork &
