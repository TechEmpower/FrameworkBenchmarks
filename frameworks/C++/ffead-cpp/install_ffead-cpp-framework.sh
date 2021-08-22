#!/bin/bash

#From https://github.com/TechEmpower/FrameworkBenchmarks/blob/master/frameworks/C%2B%2B/ulib/setup_json.sh
MAX_THREADS=$(( 3 * `nproc` / 2 ))

WRIT_THREADS=$(( $MAX_THREADS / 3 ))
SERV_THREADS=$(( $MAX_THREADS - $WRIT_THREADS ))

#git checkout e243bc096cd570cfee1edfecbcd91f4c4056fa1a -b 6.0
git clone https://github.com/sumeetchhetri/ffead-cpp
cd ffead-cpp
rm -rf .git
cd ..
mv ffead-cpp ffead-cpp-src
mv ffead-cpp-src/lang-server-backends ${IROOT}/

cd $IROOT/ffead-cpp-src/

chmod 755 *.sh resources/*.sh rtdcf/autotools/*.sh
#rm -rf web/te-benchmark-um
#rm -rf web/te-benchmark-um-pq
#rm -rf web/te-benchmark-um-mgr
#rm -rf web/te-benchmark-um-pq-async
mv ${IROOT}/server.sh script/
#mv ${IROOT}/te-benchmark-um web/
#mv ${IROOT}/te-benchmark-um-pq web/
#mv ${IROOT}/te-benchmark-um-mgr web/
#mv ${IROOT}/te-benchmark-um-pq-async web/
sed -i 's|THRD_PSIZ=6|THRD_PSIZ='${SERV_THREADS}'|g' resources/server.prop
sed -i 's|W_THRD_PSIZ=2|W_THRD_PSIZ='${WRIT_THREADS}'|g' resources/server.prop
sed -i 's|ENABLE_CRS=true|ENABLE_CRS=false|g' resources/server.prop
sed -i 's|ENABLE_SEC=true|ENABLE_SEC=false|g' resources/server.prop
sed -i 's|ENABLE_FLT=true|ENABLE_FLT=false|g' resources/server.prop
sed -i 's|ENABLE_CNT=true|ENABLE_CNT=true|g' resources/server.prop
sed -i 's|ENABLE_EXT_CNT=true|ENABLE_EXT_CNT=false|g' resources/server.prop
sed -i 's|ENABLE_CNT_MPG=true|ENABLE_CNT_MPG=false|g' resources/server.prop
sed -i 's|ENABLE_CNT_PTH=true|ENABLE_CNT_PTH=false|g' resources/server.prop
sed -i 's|ENABLE_CNT_EXT=true|ENABLE_CNT_EXT=false|g' resources/server.prop
sed -i 's|ENABLE_CNT_RST=true|ENABLE_CNT_RST=true|g' resources/server.prop
sed -i 's|ENABLE_EXT=true|ENABLE_EXT=true|g' resources/server.prop
sed -i 's|ENABLE_SCR=true|ENABLE_SCR=false|g' resources/server.prop
sed -i 's|ENABLE_SWS=true|ENABLE_SWS=false|g' resources/server.prop
sed -i 's|ENABLE_JOBS=true|ENABLE_JOBS=false|g' resources/server.prop
sed -i 's|LOGGING_ENABLED=true|LOGGING_ENABLED=false|g' resources/server.prop
sed -i 's|EVH_SINGLE=true|EVH_SINGLE=false|g' resources/server.prop

rm -rf web/default web/oauthApp web/flexApp web/markers web/te-benchmark web/peer-server

sed -i 's|localhost|tfb-database|g' web/te-benchmark-um/config/sdormmongo.xml
sed -i 's|localhost|tfb-database|g' web/te-benchmark-um/config/sdormmysql.xml
sed -i 's|localhost|tfb-database|g' web/te-benchmark-um/config/sdormpostgresql.xml
sed -i 's|localhost|tfb-database|g' web/te-benchmark-um-pq/config/sdorm.xml
sed -i 's|localhost|tfb-database|g' web/te-benchmark-um-mgr/config/sdorm.xml
sed -i 's|localhost|tfb-database|g' web/te-benchmark-um-pq-async/config/sdorm.xml
sed -i 's|127.0.0.1|tfb-database|g' resources/sample-odbcinst.ini
sed -i 's|127.0.0.1|tfb-database|g' resources/sample-odbc.ini
sed -i 's|add_subdirectory(${PROJECT_SOURCE_DIR}/web/default)||g' CMakeLists.txt
sed -i 's|add_subdirectory(${PROJECT_SOURCE_DIR}/web/flexApp)||g' CMakeLists.txt
sed -i 's|add_subdirectory(${PROJECT_SOURCE_DIR}/web/oauthApp)||g' CMakeLists.txt
sed -i 's|add_subdirectory(${PROJECT_SOURCE_DIR}/web/markers)||g' CMakeLists.txt
sed -i 's|add_subdirectory(${PROJECT_SOURCE_DIR}/web/te-benchmark)||g' CMakeLists.txt
sed -i 's|add_subdirectory(${PROJECT_SOURCE_DIR}/web/peer-server)||g' CMakeLists.txt
sed -i 's|install(FILES ${PROJECT_BINARY_DIR}/web/default/libdefault${LIB_EXT} DESTINATION ${PROJECT_NAME}-bin/lib)||g' CMakeLists.txt
sed -i 's|install(FILES ${PROJECT_BINARY_DIR}/web/flexApp/libflexApp${LIB_EXT} DESTINATION ${PROJECT_NAME}-bin/lib)||g' CMakeLists.txt
sed -i 's|install(FILES ${PROJECT_BINARY_DIR}/web/oauthApp/liboauthApp${LIB_EXT} DESTINATION ${PROJECT_NAME}-bin/lib)||g' CMakeLists.txt
sed -i 's|install(FILES ${PROJECT_BINARY_DIR}/web/markers/libmarkers${LIB_EXT} DESTINATION ${PROJECT_NAME}-bin/lib)||g' CMakeLists.txt
sed -i 's|install(FILES ${PROJECT_BINARY_DIR}/web/te-benchmark/libte-benchmark${LIB_EXT} DESTINATION ${PROJECT_NAME}-bin/lib)||g' CMakeLists.txt
sed -i 's|install(FILES ${PROJECT_BINARY_DIR}/web/peer-server/libpeer-server${LIB_EXT} DESTINATION ${PROJECT_NAME}-bin/lib)||g' CMakeLists.txt
sed -i 's|web/default/src/autotools/Makefile||g' configure.ac
sed -i 's|web/flexApp/src/autotools/Makefile||g' configure.ac
sed -i 's|web/oauthApp/src/autotools/Makefile||g' configure.ac
sed -i 's|web/markers/src/autotools/Makefile||g' configure.ac
sed -i 's|web/te-benchmark/src/autotools/Makefile||g' configure.ac
sed -i 's|web/peer-server/src/autotools/Makefile||g' configure.ac

#./autogen.sh
#./configure --enable-debug=no --enable-apachemod=yes --enable-nginxmod=yes --enable-mod_sdormmongo=yes --enable-mod_sdormsql=yes --enable-mod_rediscache=yes --enable-mod_memcached=yes CPPFLAGS="$CPPFLAGS -I${IROOT}/include/libmongoc-1.0 -I${IROOT}/include/libbson-1.0 -I${IROOT}/include/" LDFLAGS="$LDFLAGS -L${IROOT} -L${IROOT}/lib"
cmake -DSRV_ALL=on -DCINATRA_INCLUDES=${IROOT}/cinatra/include -DMOD_APACHE=on -DMOD_NGINX=on -DMOD_MEMCACHED=on -DMOD_REDIS=on -DMOD_SDORM_MONGO=on  -DDEBUG=${DEBUG} .

cp resources/sample-odbcinst.ini ${IROOT}/odbcinst.ini
cp resources/sample-odbc.ini ${IROOT}/odbc.ini

#Start building for mongodb as the World model is different for SQL use case
cd ${IROOT}/ffead-cpp-src/
cp -f web/te-benchmark-um/sql-src/TeBkUmWorldmongo.h web/te-benchmark-um/include/TeBkUmWorld.h
cp -f web/te-benchmark-um/sql-src/TeBkUmWorldmongo.cpp web/te-benchmark-um/src/TeBkUmWorld.cpp
make install -j${MAX_THREADS}

rm -f /usr/local/lib/libffead-*
rm -f /usr/local/lib/libte_benc*
rm -f /usr/local/lib/libinter.so
rm -f /usr/local/lib/libdinter.so

if [ ! -d "ffead-cpp-6.0-bin" ]
then
	exit 1
fi

cd ffead-cpp-6.0-bin
#cache related dockerfiles will add the cache.xml accordingly whenever needed
chmod 755 *.sh resources/*.sh rtdcf/autotools/*.sh
./server.sh &
COUNTER=0
while [ ! -f lib/libinter.so ]
do
    sleep 1
    COUNTER=$((COUNTER+1))
    if [ "$COUNTER" = 120 ]
    then
    	cat logs/jobs.log
    	echo "ffead-cpp exiting exiting due to failure...."
    	exit 1
    fi
done
COUNTER=0
while [ ! -f lib/libdinter.so ]
do
    sleep 1
    COUNTER=$((COUNTER+1))
    if [ "$COUNTER" = 120 ]
    then
    	cat logs/jobs.log
    	echo "ffead-cpp exiting exiting due to failure....ddlib"
    	exit 1
    fi
done
echo "ffead-cpp start successful"
sleep 5
cd tests && rm -f test.csv && cp ${IROOT}/ffead-cpp-src/tests/test-te.csv test.csv && chmod +x *.sh && ./runTests.sh
echo "ffead-cpp normal shutdown"
pkill ffead-cpp

cd ${IROOT}/ffead-cpp-src/
cp -rf ffead-cpp-6.0-bin ${IROOT}/ffead-cpp-6.0
rm -rf ffead-cpp-6.0-bin
mv ${IROOT}/nginxfc ${IROOT}/nginx-ffead-mongo

cd ${IROOT}/ffead-cpp-6.0

chmod 755 *.sh resources/*.sh rtdcf/autotools/*.sh
chmod 755 *.sh
rm -f *.cntrl
rm -f tmp/*.sess
#Done building for mongodb


#Start building for sql as the World model is different for mongodb use case
cd ${IROOT}/ffead-cpp-src/
cp -f web/te-benchmark-um/sql-src/TeBkUmWorldsql.h web/te-benchmark-um/include/TeBkUmWorld.h
cp -f web/te-benchmark-um/sql-src/TeBkUmWorldsql.cpp web/te-benchmark-um/src/TeBkUmWorld.cpp
make install -j${MAX_THREADS}

if [ ! -d "ffead-cpp-6.0-bin" ]
then
	exit 1
fi

cd ffead-cpp-6.0-bin
#cache related dockerfiles will add the cache.xml accordingly whenever needed
chmod 755 *.sh resources/*.sh rtdcf/autotools/*.sh
./server.sh &
COUNTER=0
while [ ! -f lib/libinter.so ]
do
    sleep 1
    COUNTER=$((COUNTER+1))
    if [ "$COUNTER" = 120 ]
    then
    	cat logs/jobs.log
    	echo "ffead-cpp exiting exiting due to failure...."
    	exit 1
    fi
done
COUNTER=0
while [ ! -f lib/libdinter.so ]
do
    sleep 1
    COUNTER=$((COUNTER+1))
    if [ "$COUNTER" = 120 ]
    then
    	cat logs/jobs.log
    	echo "ffead-cpp exiting exiting due to failure....ddlib"
    	exit 1
    fi
done
echo "ffead-cpp start successful"
sleep 5
cd tests && rm -f test.csv && cp ${IROOT}/ffead-cpp-src/tests/test-te.csv test.csv && chmod +x *.sh && ./runTests.sh
echo "ffead-cpp normal shutdown"
pkill ffead-cpp

cd ${IROOT}/ffead-cpp-src/
cp -rf ffead-cpp-6.0-bin ${IROOT}/ffead-cpp-6.0-sql
rm -rf ffead-cpp-6.0-bin
mv ${IROOT}/nginxfc ${IROOT}/nginx-ffead-sql

cd ${IROOT}/ffead-cpp-6.0-sql

chmod 755 *.sh resources/*.sh rtdcf/autotools/*.sh
chmod 755 *.sh
rm -f *.cntrl
rm -f tmp/*.sess
#Done building for sql
