#!/bin/bash

fw_depends wt

g++-6 \
  -std=c++14 \
  -O3 -march=native -DNDEBUG \
  -I${BOOST_INC} \
  -L${BOOST_LIB} \
  -I${WT_INC} \
  -L${WT_LIB} \
  -o te-benchmark.wt \
  benchmark.cpp \
  -lwthttp -lwt \
  -lwtdbo -lwtdbomysql \
  -lboost_system \
  -lboost_program_options \
  -lboost_thread \
  -lboost_filesystem \
  -lpthread \
  -lmysqlclient

./te-benchmark.wt -c wt_config.xml -t ${CPU_COUNT} --docroot . --approot . --http-listen 0.0.0.0:8080 --accesslog=- --no-compression
