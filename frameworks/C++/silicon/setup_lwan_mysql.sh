#! /bin/bash

fw_depends silicon lwan

rm -rf build
mkdir build
cd build
cmake .. -DCMAKE_CXX_COMPILER=clang++
make silicon_lwan_mysql

$TROOT/build/silicon_lwan_mysql ${DBHOST} 8080 &
