#!/bin/bash

rm -rf results

./benchmark_server.sh apache
sleep 10
./benchmark_server.sh sunhttp
sleep 10
./benchmark_server.sh jetty
sleep 10
./benchmark_server.sh netty
sleep 10
./benchmark_server.sh undertow

cd results
echo "name,json,plaintext,fortunes,queries,updates,db" > results
for filename in *.csv; do
    echo ${filename},`cat ${filename}` >> results
done
mv results ../results.csv
cd ..

rm -rf results
