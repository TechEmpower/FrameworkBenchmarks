#!/bin/bash

fw_depends postgresql ffead-cpp-apache

export FFEAD_CPP_PATH=${IROOT}/httpd/htdocs/ffead-cpp-2.0
export LD_LIBRARY_PATH=$IROOT:$FFEAD_CPP_PATH/lib:$LD_LIBRARY_PATH
echo $FFEAD_CPP_PATH
echo $LD_LIBRARY_PATH
rm -f $FFEAD_CPP_PATH/*.cntrl
rm -f $FFEAD_CPP_PATH/tmp/*.sess
cp $FFEAD_CPP_PATH/web/te-benchmark/config/sdormpostgresql.xml $FFEAD_CPP_PATH/web/te-benchmark/config/sdorm.xml
rm -rf $FFEAD_CPP_PATH/lib
cp -Rf $FFEAD_CPP_PATH/libsql $FFEAD_CPP_PATH/lib
cp $FFEAD_CPP_PATH/web/te-benchmark/sql-src/TeBkWorldsql.h $FFEAD_CPP_PATH/web/te-benchmark/include/TeBkWorld.h
#chown -R www-data:www-data $FFEAD_CPP_PATH
export ODBCINI=$IROOT/odbc.ini
export ODBCSYSINI=$IROOT
${IROOT}/httpd/bin/apachectl start > ffead.log 2>&1
