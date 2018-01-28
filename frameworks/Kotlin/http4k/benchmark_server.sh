#!/bin/bash

SERVER=$1

mkdir results

docker rm -fv $(docker ps -aq)
docker run  -p 5432:5432 --name db -d dbi

sleep 10

./gradlew clean ${SERVER}:uber

java -server -XX:+UseNUMA -XX:+UseParallelGC -XX:+AggressiveOpts -XX:+AlwaysPreTouch -jar ${SERVER}/build/libs/http4k-${SERVER}-benchmark.jar &

sleep 10

function abTest {
    ab -n 5000 -c 25 http://localhost:9000/$1 | grep "Requests per second" | awk '{ print $4 }'
}

echo `abTest "json"`, \
     `abTest "plaintext"`,  \
     `abTest "fortunes"`,  \
     `abTest "queries\?queries\=12"`,  \
     `abTest "updates\?queries\=12"`,  \
     `abTest "db"` > results/${SERVER}.csv

kill -9 `ps -ef | grep java | grep PreT | awk '{ print $2 }'`

docker rm -fv $(docker ps -aq)
