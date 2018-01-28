#!/bin/bash

dkill
./run_db.sh

sleep 10

./benchmark_server.sh sunhttp
./benchmark_server.sh apache
./benchmark_server.sh jetty
./benchmark_server.sh netty
./benchmark_server.sh undertow

dkill