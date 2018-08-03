#!/bin/bash

#From https://github.com/TechEmpower/FrameworkBenchmarks/blob/master/frameworks/C%2B%2B/ulib/setup_json.sh
MAX_THREADS=$(( 3 * `nproc` / 2 ))

WRIT_THREADS=$(( $MAX_THREADS / 3 ))
SERV_THREADS=$(( $MAX_THREADS - $WRIT_THREADS ))

cd $IROOT

wget -q https://github.com/sumeetchhetri/ffead-cpp/archive/master.zip
unzip master.zip
mv ffead-cpp-master ffead-cpp-src
cd ffead-cpp-src/

chmod 755 *.sh resources/*.sh rtdcf/autotools/*.sh
rm -rf web/te-benchmark
cp -f ${TROOT}/server.sh script/
cp -rf ${TROOT}/te-benchmark web/
sed -i 's|THRD_PSIZ=6|THRD_PSIZ='${SERV_THREADS}'|g' resources/server.prop
sed -i 's|W_THRD_PSIZ=2|W_THRD_PSIZ='${WRIT_THREADS}'|g' resources/server.prop
sed -i 's|LOGGING_ENABLED=true|LOGGING_ENABLED=false|g' resources/server.prop

./autogen.sh
./configure --enable-debug=no --enable-apachemod=yes --enable-nginxmod=yes --enable-mod_sdormmongo=yes --enable-mod_sdormsql=yes --enable-mod_rediscache=yes --enable-mod_memcached=yes CPPFLAGS="$CPPFLAGS -I${IROOT}/include/libmongoc-1.0 -I${IROOT}/include/libbson-1.0 -I${IROOT}/include/" LDFLAGS="$LDFLAGS -L${IROOT} -L${IROOT}/lib"
make install

rm -rf web/default web/oauthApp web/flexApp web/markers

sed -i 's|localhost|tfb-database|g' web/te-benchmark/config/sdorm.xml
sed -i 's|localhost|tfb-database|g' web/te-benchmark/config/sdormmongo.xml
sed -i 's|localhost|tfb-database|g' web/te-benchmark/config/sdormmysql.xml
sed -i 's|localhost|tfb-database|g' web/te-benchmark/config/sdormpostgresql.xml
sed -i 's|127.0.0.1|tfb-database|g' resources/sample-odbcinst.ini
sed -i 's|127.0.0.1|tfb-database|g' resources/sample-odbc.ini

cp resources/sample-odbcinst.ini ${IROOT}/odbcinst.ini
cp resources/sample-odbc.ini ${IROOT}/odbc.ini

cd ${IROOT}/ffead-cpp-src/

#Build for mongodb first
cp -f web/te-benchmark/sql-src/TeBkWorldmongo.h web/te-benchmark/include/TeBkWorld.h
cp -f web/te-benchmark/sql-src/TeBkWorldmongo.cpp web/te-benchmark/src/TeBkWorld.cpp
make build-apps
cd ffead-cpp-2.0-bin
chmod 755 *.sh resources/*.sh rtdcf/autotools/*.sh
./server.sh &
while [ ! -f lib/libinter.so ]
do
	sleep 1
done
while [ ! -f lib/libdinter.so ]
do
	sleep 1
done
pkill CHS
mv lib ../lib-mongo

cd ${IROOT}/ffead-cpp-src/
#Build for sql now
cp -f web/te-benchmark/sql-src/TeBkWorldsql.h web/te-benchmark/include/TeBkWorld.h
cp -f web/te-benchmark/sql-src/TeBkWorldsql.cpp web/te-benchmark/src/TeBkWorld.cpp
make build-apps
cd ffead-cpp-2.0-bin
chmod 755 *.sh resources/*.sh rtdcf/autotools/*.sh
./server.sh &
while [ ! -f lib/libinter.so ]
do
	sleep 1
done
while [ ! -f lib/libdinter.so ]
do
	sleep 1
done
pkill CHS
mv lib ../lib-sql

cd ${IROOT}/ffead-cpp-src/
cp -rf ffead-cpp-2.0-bin ${IROOT}/ffead-cpp-2.0
cp -rf lib-mongo ${IROOT}/ffead-cpp-2.0/
cp -rf lib-sql ${IROOT}/ffead-cpp-2.0/
cp -rf lib-mongo ${IROOT}/ffead-cpp-2.0/lib
rm -rf ffead-cpp-2.0-bin

cd ${IROOT}/ffead-cpp-2.0
cp -f ${TROOT}/run_ffead.sh ./

chmod 755 *.sh resources/*.sh rtdcf/autotools/*.sh
chmod 755 $FFEAD_CPP_PATH/*.sh
rm -f $FFEAD_CPP_PATH/*.cntrl
rm -f $FFEAD_CPP_PATH/tmp/*.sess
#cache related dockerfiles will add the cache.xml accordingly whenever needed
rm -f web/te-benchmark/config/cache.xml
