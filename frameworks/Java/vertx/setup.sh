#!/bin/bash
<<<<<<< HEAD

sed -i 's|host: \x27.*\x27|host: \x27'"${DBHOST}"'\x27|g' app.js

fw_depends java7 vertx 

vertx run app.js &
=======
# load java environment variables
source $IROOT/java8.installed

sed -i 's|host: \x27.*\x27|host: \x27'"${DBHOST}"'\x27|g' app.js

export JAVA_OPTS="-server -XX:+UseNUMA -XX:+UseParallelGC -XX:+AggressiveOpts"
${IROOT}/vert.x-2.1.5/bin/vertx run app.js &
>>>>>>> master
