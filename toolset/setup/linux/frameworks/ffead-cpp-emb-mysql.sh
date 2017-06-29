#!/bin/bash

fw_installed ffead-cpp-emb-mysql && return 0

fw_depends mysql
fw_depends ffead-cpp-unixodbc
fw_depends ffead-cpp-mongocdriver
fw_depends ffead-cpp-framework

cd ${IROOT}/ffead-cpp-src/
cp -f web/te-benchmark/sql-src/TeBkWorldsql.h web/te-benchmark/include/TeBkWorld.h
cp -f web/te-benchmark/sql-src/TeBkWorldsql.cpp web/te-benchmark/src/TeBkWorld.cpp
cp -f web/te-benchmark/config/sdormmysql.xml web/te-benchmark/config/sdorm.xml
rm -rf ffead-cpp-2.0-bin
make build-apps
rm -rf ${IROOT}/ffead-cpp-2.0
cp -rf ffead-cpp-2.0-bin ${IROOT}/ffead-cpp-2.0
cd ${IROOT}/ffead-cpp-2.0
rm -rf web/default web/oauthApp web/flexApp web/markers
chmod 755 *.sh resources/*.sh rtdcf/autotools/*.sh
./server.sh > ffead-cpp-emb-mysql.log 2>&1
while ! echo exit | nc localhost 8080; do sleep 5; done
rm -f serv.ctrl
sleep 10

cd ${IROOT}

echo -e "export PATH=${FFEAD_CPP_PATH}:\$PATH" > $IROOT/ffead-cpp-emb-mysql.installed
