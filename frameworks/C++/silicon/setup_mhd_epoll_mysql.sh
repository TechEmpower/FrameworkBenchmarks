#! /bin/bash

fw_depends silicon microhttpd

rm -rf build
mkdir build
cd build
cmake .. -DCMAKE_CXX_COMPILER=clang++-3.5
make silicon_epoll_mysql

$TROOT/build/silicon_epoll_mysql ${DBHOST} 8080 ${MAX_THREADS} &
