#!/bin/bash

fw_depends apache
fw_depends ffead-cpp-apache

export FFEAD_CPP_PATH=/var/www/ffead-cpp-2.0
export LD_LIBRARY_PATH=$IROOT:$FFEAD_CPP_PATH/lib:$LD_LIBRARY_PATH
echo $FFEAD_CPP_PATH
echo $LD_LIBRARY_PATH
sudo rm -f $FFEAD_CPP_PATH/*.cntrl
sudo rm -f $FFEAD_CPP_PATH/tmp/*.sess
sudo cp $FFEAD_CPP_PATH/web/te-benchmark/config/sdormmongo.xml $FFEAD_CPP_PATH/web/te-benchmark/config/sdorm.xml
sudo rm -rf $FFEAD_CPP_PATH/lib
sudo cp -Rf $FFEAD_CPP_PATH/libmongo $FFEAD_CPP_PATH/lib
sudo cp $FFEAD_CPP_PATH/web/te-benchmark/sql-src/TeBkWorldmongo.h $FFEAD_CPP_PATH/web/te-benchmark/include/TeBkWorld.h
sudo /etc/init.d/apache2 restart > ffead.log 2>&1
