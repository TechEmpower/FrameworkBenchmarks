#!/bin/bash

fw_installed ffead-cpp-framework && return 0

fw_get -o ffead-cpp-src.zip https://github.com/sumeetchhetri/ffead-cpp/archive/master.zip
rm -rf ffead-cpp-src
rm -rf ffead-cpp-master
unzip ffead-cpp-src.zip
mv ffead-cpp-master ffead-cpp-src
cd ffead-cpp-src/
chmod 755 *.sh resources/*.sh rtdcf/autotools/*.sh
rm -rf web/te-benchmark
cp -rf ${TROOT}/te-benchmark web/
./autogen.sh
./configure --enable-apachemod=yes --enable-mod_sdormmongo=yes --enable-mod_sdormsql=yes CPPFLAGS="$CPPFLAGS -I${IROOT}/include/libmongoc-1.0 -I${IROOT}/include/libbson-1.0 -I${IROOT}/include/" LDFLAGS="$LDFLAGS -L${IROOT}"
make install
rm -rf ${IROOT}/ffead-cpp-2.0
cp -rf ffead-cpp-2.0-bin ${IROOT}/ffead-cpp-2.0

sed -i 's|localhost|'${DBHOST}'|g' web/te-benchmark/config/sdorm.xml
sed -i 's|localhost|'${DBHOST}'|g' web/te-benchmark/config/sdormmongo.xml
sed -i 's|localhost|'${DBHOST}'|g' web/te-benchmark/config/sdormmysql.xml
sed -i 's|localhost|'${DBHOST}'|g' web/te-benchmark/config/sdormpostgresql.xml
sed -i 's|localhost|'${DBHOST}'|g' resources/sample-odbcinst.ini
sed -i 's|localhost|'${DBHOST}'|g' resources/sample-odbc.ini

cp resources/sample-odbcinst.ini ${IROOT}/odbcinst.ini
cp resources/sample-odbc.ini ${IROOT}/odbc.ini

cd ${IROOT}

touch ${IROOT}/ffead-cpp-framework.installed