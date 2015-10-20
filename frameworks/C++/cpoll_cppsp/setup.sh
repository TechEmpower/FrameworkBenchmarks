#!/bin/bash

sed -i 's|#define BENCHMARK_DB_HOST ".*"|#define BENCHMARK_DB_HOST "'"$DBHOST"'"|g' www/connectioninfo.H

fw_depends cppsp

make clean
make
cd $CPPSP_HOME
./run_application $TROOT/www -g g++-4.8 -m /forcedynamic.cppsm &
