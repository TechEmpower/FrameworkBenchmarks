#! /bin/bash

fw_depends mysql silicon microhttpd

rm -rf build
mkdir build
cd build
cmake .. -DCMAKE_CXX_COMPILER=clang++-3.9
make silicon_tpc_mysql

$TROOT/build/silicon_tpc_mysql ${DBHOST} 8080 ${CPU_COUNT} &
