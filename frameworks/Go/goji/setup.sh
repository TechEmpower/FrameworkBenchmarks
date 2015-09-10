#!/bin/bash

<<<<<<< HEAD
sed -i 's|tcp(.*:3306)|tcp('"${DBHOST}"':3306)|g' src/goji/server.go

fw_depends go

=======
export GOROOT=${IROOT}/go
>>>>>>> master
export GOPATH=${TROOT}
export GOGC=800

go get github.com/go-sql-driver/mysql
go get github.com/zenazn/goji
go get github.com/zenazn/goji/web

go run src/goji/server.go &
