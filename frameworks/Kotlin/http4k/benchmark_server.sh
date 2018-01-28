#!/bin/bash

SERVER=$1

./gradlew ${SERVER}:uber > /dev/null 2>&1

java -server -XX:+UseNUMA -XX:+UseParallelGC -XX:+AggressiveOpts -XX:+AlwaysPreTouch -jar ${SERVER}/build/libs/http4k-${SERVER}-benchmark.jar > /dev/null 2>&1 &

PID=$!

sleep 10

function abTest {
    ab -q -n 50 -c 10 http://localhost:9000/$1 | grep "Requests per second" | awk '{ print $4 }'
}

echo -e ${SERVER} '\t'  \
    '\t' json=`abTest "json"` \
    '\t' plaintext=`abTest "plaintext"` \
    '\t' fortunes=`abTest "fortunes"` \
    '\t' queries=`abTest "queries\?queries\=12"` \
    '\t' updates=`abTest "updates\?queries\=12"` \
    '\t' db=`abTest "db"`

kill -9 ${PID} > /dev/null 2>&1
