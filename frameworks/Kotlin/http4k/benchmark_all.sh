#!/bin/bash

rm -rf results

./benchmark_server.sh sunhttp
sleep 10
./benchmark_server.sh apache
sleep 10
./benchmark_server.sh jetty
sleep 10
./benchmark_server.sh netty
sleep 10
./benchmark_server.sh undertow

cat results/*.csv