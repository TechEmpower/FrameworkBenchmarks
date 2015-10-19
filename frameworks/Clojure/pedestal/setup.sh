#!/bin/bash

fw_depends java7 leiningen

sed -i 's|127.0.0.1:3306|'${DBHOST}':3306|g' src/pedestal/service.clj

lein clean

rm -rf target
# pack all dependencies into a single jar: target/pedestal-standalone.jar
lein uberjar
# -server is much faster
# 'lein run' passes '-client -XX:+TieredCompilation -XX:TieredStopAtLevel=1' which make it starts fast, but runs slow
java -server -jar target/pedestal-standalone.jar &
