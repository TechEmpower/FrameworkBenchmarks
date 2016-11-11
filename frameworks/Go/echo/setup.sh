#!/bin/bash

fw_depends go

go get github.com/labstack/echo/...
go get github.com/lib/pq
go get github.com/valyala/tcplisten
go install standard fasthttp

fasthttp &
