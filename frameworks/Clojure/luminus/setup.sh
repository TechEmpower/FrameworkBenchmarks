#!/bin/bash

fw_depends java resin leiningen

# Update db host in the source file
sed -i 's|127.0.0.1|'"${DBHOST}"'|g' hello/env/prod/resources/config.edn

cd hello
lein clean
rm -rf target
# pack all dependencies into a single jar: target/hello.jar
lein uberjar
java -server -jar target/hello.jar  &
