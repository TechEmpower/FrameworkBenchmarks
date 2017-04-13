#!/bin/bash

fw_depends mysql apache wt

sed -i 's|INSERT_DB_HOST_HERE|'"${DBHOST}"'|g' benchmark.cpp

g++-4.8 -O3 -DNDEBUG -std=c++0x -L${BOOST_LIB} -I${BOOST_INC} -L${WT_LIB} -I${WT_INC} -o benchmark.wt benchmark.cpp -lwt -lwthttp -lwtdbo -lwtdbomysql -lboost_thread -lboost_system

./benchmark.wt -c wt_config.xml -t ${CPU_COUNT} --docroot . --http-address 0.0.0.0 --http-port 8080 --accesslog=- --no-compression
