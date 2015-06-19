#! /bin/bash

fw_depends silicon microhttpd

rm -rf build
mkdir build
cd build
cmake .. -DCMAKE_CXX_COMPILER=g++-4.9
make silicon_tpc_mysql

$TROOT/build/silicon_tpc_mysql ${DBHOST} 8080 ${MAX_THREADS} &
