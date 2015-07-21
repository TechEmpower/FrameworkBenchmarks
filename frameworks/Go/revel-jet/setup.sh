#!/bin/bash
# Set the root of our go installation
export GOROOT=${IROOT}/go
export GOPATH=${TROOT}

sed -i 's|tcp(.*:3306)|tcp('"${DBHOST}"':3306)|g' src/benchmark/conf/app.conf

# here te find the go executable
export PATH="$GOROOT/bin:$PATH"

go get -u github.com/robfig/revel/revel github.com/eaigner/jet
go build -o bin/revel github.com/robfig/revel/revel
bin/revel run benchmark prod &