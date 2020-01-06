#!/bin/bash

#From https://github.com/TechEmpower/FrameworkBenchmarks/blob/master/frameworks/C%2B%2B/ulib/setup_json.sh
MAX_THREADS=$(( 3 * `nproc` / 2 ))

WRIT_THREADS=$(( $MAX_THREADS / 3 ))
SERV_THREADS=$(( $MAX_THREADS - $WRIT_THREADS ))

cd $IROOT

wget -q https://github.com/efficient/libcuckoo/archive/master.zip
unzip master.zip
rm -f master.zip
cd libcuckoo-master
cmake -DCMAKE_INSTALL_PREFIX=/usr .
make install
cd $IROOT
rm -rf libcuckoo-master

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
sed -i 's|ENABLE_CRS=true|ENABLE_CRS=false|g' resources/server.prop
sed -i 's|ENABLE_SEC=true|ENABLE_SEC=false|g' resources/server.prop
sed -i 's|ENABLE_FLT=true|ENABLE_FLT=false|g' resources/server.prop
sed -i 's|ENABLE_CNT=false|ENABLE_CNT=true|g' resources/server.prop
sed -i 's|ENABLE_CNT_MPG=true|ENABLE_CNT_MPG=false|g' resources/server.prop
sed -i 's|ENABLE_CNT_PTH=true|ENABLE_CNT_PTH=false|g' resources/server.prop
sed -i 's|ENABLE_CNT_EXT=true|ENABLE_CNT_EXT=false|g' resources/server.prop
sed -i 's|ENABLE_CNT_RST=false|ENABLE_CNT_RST=true|g' resources/server.prop
sed -i 's|ENABLE_EXT=false|ENABLE_EXT=true|g' resources/server.prop
sed -i 's|ENABLE_SCR=true|ENABLE_SCR=false|g' resources/server.prop
sed -i 's|ENABLE_SWS=true|ENABLE_SWS=false|g' resources/server.prop
sed -i 's|LOGGING_ENABLED=true|LOGGING_ENABLED=false|g' resources/server.prop

rm -rf web/default web/oauthApp web/flexApp web/markers

sed -i 's|localhost|tfb-database|g' web/te-benchmark/config/sdorm.xml
sed -i 's|localhost|tfb-database|g' web/te-benchmark/config/sdormmongo.xml
sed -i 's|localhost|tfb-database|g' web/te-benchmark/config/sdormmysql.xml
sed -i 's|localhost|tfb-database|g' web/te-benchmark/config/sdormpostgresql.xml
sed -i 's|127.0.0.1|tfb-database|g' resources/sample-odbcinst.ini
sed -i 's|127.0.0.1|tfb-database|g' resources/sample-odbc.ini
sed -i 's|add_subdirectory(${PROJECT_SOURCE_DIR}/web/default)||g' CMakeLists.txt
sed -i 's|add_subdirectory(${PROJECT_SOURCE_DIR}/web/flexApp)||g' CMakeLists.txt
sed -i 's|add_subdirectory(${PROJECT_SOURCE_DIR}/web/oauthApp)||g' CMakeLists.txt
sed -i 's|add_subdirectory(${PROJECT_SOURCE_DIR}/web/markers)||g' CMakeLists.txt
sed -i 's|install(FILES ${PROJECT_SOURCE_DIR}/web/default/libdefault${CMAKE_SHARED_LIBRARY_SUFFIX} DESTINATION ${PROJECT_NAME}-bin/lib)||g' CMakeLists.txt
sed -i 's|install(FILES ${PROJECT_SOURCE_DIR}/web/flexApp/libflexApp${CMAKE_SHARED_LIBRARY_SUFFIX} DESTINATION ${PROJECT_NAME}-bin/lib)||g' CMakeLists.txt
sed -i 's|install(FILES ${PROJECT_SOURCE_DIR}/web/oauthApp/liboauthApp${CMAKE_SHARED_LIBRARY_SUFFIX} DESTINATION ${PROJECT_NAME}-bin/lib)||g' CMakeLists.txt
sed -i 's|install(FILES ${PROJECT_SOURCE_DIR}/web/markers/libmarkers${CMAKE_SHARED_LIBRARY_SUFFIX} DESTINATION ${PROJECT_NAME}-bin/lib)||g' CMakeLists.txt
sed -i 's|web/default/src/autotools/Makefile||g' configure.ac
sed -i 's|web/flexApp/src/autotools/Makefile||g' configure.ac
sed -i 's|web/oauthApp/src/autotools/Makefile||g' configure.ac
sed -i 's|web/markers/src/autotools/Makefile||g' configure.ac

#./autogen.sh
#./configure --enable-debug=no --enable-apachemod=yes --enable-nginxmod=yes --enable-mod_sdormmongo=yes --enable-mod_sdormsql=yes --enable-mod_rediscache=yes --enable-mod_memcached=yes CPPFLAGS="$CPPFLAGS -I${IROOT}/include/libmongoc-1.0 -I${IROOT}/include/libbson-1.0 -I${IROOT}/include/" LDFLAGS="$LDFLAGS -L${IROOT} -L${IROOT}/lib"
#make install
cmake -DMOD_APACHE=on -DMOD_NGINX=on -DMOD_MEMCACHED=on -DMOD_REDIS=on -DMOD_SDORM_MONGO=on .
#make install -j4

cp resources/sample-odbcinst.ini ${IROOT}/odbcinst.ini
cp resources/sample-odbc.ini ${IROOT}/odbc.ini

cd ${IROOT}/ffead-cpp-src/

make install -j4
cd ffead-cpp-3.0-bin
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
pkill ffead-cpp

cd ${IROOT}/ffead-cpp-src/
cp -rf ffead-cpp-3.0-bin ${IROOT}/ffead-cpp-3.0
rm -rf ffead-cpp-3.0-bin

ln -s ${IROOT}/ffead-cpp-3.0/lib/libte_benchmark.so /usr/local/lib/libte_benchmark.so
ln -s ${IROOT}/ffead-cpp-3.0/lib/libffead-modules.so /usr/local/lib/libffead-modules.so
ln -s ${IROOT}/ffead-cpp-3.0/lib/libffead-framework.so /usr/local/lib/libffead-framework.so
ln -s ${IROOT}/ffead-cpp-3.0/lib/libinter.so /usr/local/lib/libinter.so
ln -s ${IROOT}/ffead-cpp-3.0/lib/libdinter.so /usr/local/lib/libdinter.so
ldconfig

cd ${IROOT}/ffead-cpp-3.0
cp -f ${TROOT}/run_ffead.sh ./

chmod 755 *.sh resources/*.sh rtdcf/autotools/*.sh
chmod 755 $FFEAD_CPP_PATH/*.sh
rm -f $FFEAD_CPP_PATH/*.cntrl
rm -f $FFEAD_CPP_PATH/tmp/*.sess
#cache related dockerfiles will add the cache.xml accordingly whenever needed
rm -f web/te-benchmark/config/cache.xml
