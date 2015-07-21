#!/bin/bash
export CPPSP_HOME=$IROOT/cppsp_0.2.3

sed -i 's|#define BENCHMARK_DB_HOST ".*"|#define BENCHMARK_DB_HOST "'"$DBHOST"'"|g' www/connectioninfo.H

export CPLUS_INCLUDE_PATH=/usr/include/postgresql:/usr/include/postgresql/9.3/server:$CPLUS_INCLUDE_PATH

make clean
make
cd $CPPSP_HOME
./run_application $TROOT/www -g g++-4.8 -m /forcedynamic.cppsm &