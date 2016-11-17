#!/bin/bash

fw_depends ffead-cpp-nginx

export FFEAD_CPP_PATH=$TROOT/ffead-cpp-2.0
export LD_LIBRARY_PATH=$IROOT:$FFEAD_CPP_PATH/lib:$LD_LIBRARY_PATH
echo $FFEAD_CPP_PATH
echo $LD_LIBRARY_PATH
rm -f $FFEAD_CPP_PATH/*.cntrl
rm -f $FFEAD_CPP_PATH/tmp/*.sess
cp $FFEAD_CPP_PATH/web/te-benchmark/config/sdormmongo.xml $FFEAD_CPP_PATH/web/te-benchmark/config/sdorm.xml
rm -rf $FFEAD_CPP_PATH/lib
cp -Rf $FFEAD_CPP_PATH/libmongo $FFEAD_CPP_PATH/lib
cp $FFEAD_CPP_PATH/web/te-benchmark/sql-src/TeBkWorldmongo.h $FFEAD_CPP_PATH/web/te-benchmark/include/TeBkWorld.h
$IROOT/nginxfc/sbin/nginx > ffead.log 2>&1
