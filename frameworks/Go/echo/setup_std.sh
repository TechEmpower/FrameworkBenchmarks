#!/bin/bash

fw_depends go

go get ./...
go install ./...

standard &
