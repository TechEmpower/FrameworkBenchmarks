#!/bin/bash

fw_depends java resin leiningen

# Update db host in the source file
sed -i 's|:jdbc-url   "jdbc:postgresql://.*/hello_world|:jdbc-url   "jdbc:postgresql://'"${DBHOST}"':5432/hello_world|g' hello/src/hello/db/core.clj

cd hello
lein clean
rm -rf target
# pack all dependencies into a single jar: target/hello.jar
lein uberjar
java -server -jar target/hello.jar  &
