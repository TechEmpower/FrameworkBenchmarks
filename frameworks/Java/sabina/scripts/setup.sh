#!/bin/bash

fw_depends java8 maven

echo "PARAMS: $@"

gradle/wrapper clean distZip $@ -Ddb.host=${DBHOST}

echo "STATUS GRADLE: $#"

unzip -d build/ build/distributions/sabina-1.0.0.zip

echo "STATUS UNZIP: $#"

build/sabina-1.0.0/bin/sabina &

echo "STATUS SABINA: $#"


