#!/bin/bash

#From https://github.com/TechEmpower/FrameworkBenchmarks/blob/master/frameworks/C%2B%2B/ulib/setup_json.sh
MAX_THREADS=$(( 3 * nproc / 2 ))

WRIT_THREADS=$(( $MAX_THREADS / 3 ))
SERV_THREADS=$(( $MAX_THREADS - $WRIT_THREADS ))

cd $IROOT

wget -q https://github.com/sumeetchhetri/ffead-cpp/archive/master.zip
unzip master.zip
mv ffead-cpp-master ffead-cpp-src
cd ffead-cpp-src/

#
# In the current version of the ffead-cpp framework, when the framework figures
# out the column name for each data entity property, it forces all the column
# names to lowercase.  This is a problem for our MongoDB tests because it will
# lead to records being written to the "world" collection that have a
# "randomnumber" attribute rather than the expected "randomNumber" (capital "N")
# even though we specify the correct capitalization in the test implementation.
# This causes the implementation to fail the updates test verification.
#
# There doesn't seem to be any way to work around this problem in the test
# implementation, so instead we modify the framework, making it trust the
# provided capitalization of all column names.
#
# TODO: Address this problem in the ffead-cpp framework itself.
#
sed -i 's|toLowerCopy|trimCopy|g' src/modules/sdorm/DataSourceMapping.cpp

chmod 755 *.sh resources/*.sh rtdcf/autotools/*.sh
rm -rf web/te-benchmark
cp -f ${TROOT}/server.sh script/
cp -rf ${TROOT}/te-benchmark web/
sed -i 's|THRD_PSIZ=6|THRD_PSIZ='${SERV_THREADS}'|g' resources/server.prop
sed -i 's|W_THRD_PSIZ=2|W_THRD_PSIZ='${WRIT_THREADS}'|g' resources/server.prop
sed -i 's|LOGGING_ENABLED=true|LOGGING_ENABLED=false|g' resources/server.prop
sed -i 's|localhost|tfb-database|g' resources/server.prop
./autogen.sh
./configure --enable-apachemod=yes --enable-mod_sdormmongo=yes --enable-mod_sdormsql=yes CPPFLAGS="$CPPFLAGS -I${IROOT}/include/libmongoc-1.0 -I${IROOT}/include/libbson-1.0 -I${IROOT}/include/" LDFLAGS="$LDFLAGS -L${IROOT}"
make install
rm -rf ${IROOT}/ffead-cpp-2.0
cp -rf ffead-cpp-2.0-bin ${IROOT}/ffead-cpp-2.0

sed -i 's|localhost|tfb-database|g' web/te-benchmark/config/sdorm.xml
sed -i 's|localhost|tfb-database|g' web/te-benchmark/config/sdormmongo.xml
sed -i 's|localhost|tfb-database|g' web/te-benchmark/config/sdormmysql.xml
sed -i 's|localhost|tfb-database|g' web/te-benchmark/config/sdormpostgresql.xml
sed -i 's|localhost|tfb-database|g' resources/sample-odbcinst.ini
sed -i 's|localhost|tfb-database|g' resources/sample-odbc.ini

cp resources/sample-odbcinst.ini ${IROOT}/odbcinst.ini
cp resources/sample-odbc.ini ${IROOT}/odbc.ini
