#!/bin/bash
source $IROOT/java7.installed
source $IROOT/lein.installed

# Update db host in the source file
sed -i 's|:subname "//.*:3306|:subname "//'"${DBHOST}"':3306|g' hello/src/hello/db/core.clj

cd hello
lein clean
rm -rf target
# pack all dependencies into a single jar: target/http-kit-standalone.jar
lein uberjar
java -server -jar target/hello.jar  &
