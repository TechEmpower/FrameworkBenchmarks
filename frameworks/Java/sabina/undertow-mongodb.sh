#!/bin/bash

fw_depends java8

# load java environment variables
source $IROOT/java8.installed

echo "PARAMS: $@"

gradle/wrapper clean distZip -Dsabina.backend=undertow -Dsabina.benchmark.repository=mongodb -Ddb.host=${DBHOST}

echo "STATUS GRADLE: $#"

unzip -d build/ build/distributions/sabina-1.0.0.zip

echo "STATUS UNZIP: $#"

build/sabina-1.0.0/bin/sabina &

echo "STATUS SABINA: $#"


