#!/bin/bash

fw_depends mysql java8 leiningen

sed -i 's|127.0.0.1:3306|'${DBHOST}':3306|g' src/pedestal/pdg.clj

lein clean

rm -rf target
# pack all dependencies into a single jar: target/pedestal-standalone.jar
lein uberjar

# java -jar target/pedestal-standalone.jar &
# java -XX:+AggressiveOpts -jar -D"io.pedestal.log.defaultMetricsRecorder=nil" -D"io.pedestal.log.overrideLogger=nil" target/pedestal-standalone.jar &
#java -jar -D"io.pedestal.log.defaultMetricsRecorder=nil" -D"io.pedestal.log.overrideLogger=nil" target/pedestal-standalone.jar > /dev/null 2>&1 &
java -jar -D"io.pedestal.log.defaultMetricsRecorder=nil" -D"io.pedestal.log.overrideLogger=nil" target/pedestal-standalone.jar &

