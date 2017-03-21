#! /bin/bash

fw_depends mysql silicon microhttpd

rm -rf build
mkdir build
cd build
cmake .. -DCMAKE_CXX_COMPILER=clang++-3.9
make silicon_epoll_mysql

$TROOT/build/silicon_epoll_mysql ${DBHOST} 8080 ${MAX_THREADS} &
