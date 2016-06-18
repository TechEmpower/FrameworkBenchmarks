#!/bin/bash

fw_depends leiningen java

sed -i 's|127.0.0.1|'"${DBHOST}"'|g' hello/src/hello/handler.clj

cd hello
lein clean
lein deps
rm -rf target
# pack all dependencies into a single jar: target/http-kit-standalone.jar
lein uberjar
# -server is much faster
# 'lein run' passes '-client -XX:+TieredCompilation -XX:TieredStopAtLevel=1' which make it starts fast, but runs slow
java -server -jar target/http-kit-standalone.jar  &
