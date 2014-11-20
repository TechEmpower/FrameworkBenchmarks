#!/bin/bash

# This has to be included here since it is one of the vars NOT copied from the original environment
export LD_LIBRARY_PATH="${BOOST_LIB}:${WT_LIB}:${LD_LIBRARY_PATH}"

sed -i 's|INSERT_DB_HOST_HERE|'"${DBHOST}"'|g' benchmark.cpp

g++-4.8 -O3 -DNDEBUG -std=c++0x -L${BOOST_LIB} -I${BOOST_INC} -L${WT_LIB} -I${WT_INC} -o benchmark.wt benchmark.cpp -lwt -lwthttp -lwtdbo -lwtdbomysql -lboost_thread

./benchmark_postgres.wt -c wt_config.xml -t ${MAX_THREADS} --docroot . --http-address 0.0.0.0 --http-port 8080 --accesslog=- --no-compression &