#! /bin/bash
 
fw_depends mysql silicon lwan

rm -rf build
mkdir build
cd build
cmake .. -DCMAKE_CXX_COMPILER=clang++-3.9
make silicon_lwan_mysql

$TROOT/build/silicon_lwan_mysql ${DBHOST} 8080 &
