#!/bin/bash

fw_depends postgresql apache wt

sed -i 's|INSERT_DB_HOST_HERE|'"${DBHOST}"'|g' benchmark.cpp

g++-4.8 -O3 -DNDEBUG -DBENCHMARK_USE_POSTGRES -std=c++0x -L${BOOST_LIB} -I${BOOST_INC} -L${WT_LIB} -I${WT_INC} -o benchmark_postgres.wt benchmark.cpp -lwt -lwthttp -lwtdbo -lwtdbopostgres -lboost_thread -lboost_system

./benchmark_postgres.wt -c wt_config.xml -t ${CPU_COUNT} --docroot . --http-address 0.0.0.0 --http-port 8080 --accesslog=- --no-compression
