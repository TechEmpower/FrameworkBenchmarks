#!/bin/bash
# load java environment variables
source $IROOT/java8.installed

sed -i 's|host: \x27.*\x27|host: \x27'"${DBHOST}"'\x27|g' app.js

export JAVA_OPTS="-server -XX:+UseNUMA -XX:+UseParallelGC -XX:+AggressiveOpts"
${IROOT}/vert.x-2.1.5/bin/vertx run app.js &
