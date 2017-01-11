#!/bin/bash

fw_installed ffead-cpp && return 0

sudo apt-get remove -y libodbc1 unixodbc unixodbc-dev

fw_get -o unixODBC-2.3.4.tar.gz ftp://ftp.unixodbc.org/pub/unixODBC/unixODBC-2.3.4.tar.gz
fw_untar unixODBC-2.3.4.tar.gz
cd unixODBC-2.3.4
./configure --enable-stats=no --enable-gui=no --enable-drivers=no --enable-iconv --with-iconv-char-enc=UTF8 --with-iconv-ucode-enc=UTF16LE --libdir=${IROOT} --prefix=${IROOT} --sysconfdir=${IROOT}
make install
cd -

sudo apt-get install -y build-essential
sudo apt-get install -y uuid-dev libmyodbc odbc-postgresql

if [ ! -d "${IROOT}/ffead-cpp-2.0" ]; then
	fw_get -o ffead-cpp-2.0.tar.gz https://github.com/sumeetchhetri/ffead-cpp/releases/download/2.0/ffead-cpp-2.0-te-bin.tar.gz
	fw_untar ffead-cpp-2.0.tar.gz

	rm -rf ${IROOT}/ffead-cpp-2.0
	#cp -R ffead-cpp-2.0-bin/ ${IROOT}
	mv ${IROOT}/ffead-cpp-2.0-bin ${IROOT}/ffead-cpp-2.0
	#rm -rf ffead-cpp-2.0/
fi

sed -i 's|localhost|'${DBHOST}'|g' ${IROOT}/ffead-cpp-2.0/web/te-benchmark/config/sdorm.xml
sed -i 's|localhost|'${DBHOST}'|g' ${IROOT}/ffead-cpp-2.0/web/te-benchmark/config/sdormmongo.xml
sed -i 's|localhost|'${DBHOST}'|g' ${IROOT}/ffead-cpp-2.0/web/te-benchmark/config/sdormmysql.xml
sed -i 's|localhost|'${DBHOST}'|g' ${IROOT}/ffead-cpp-2.0/web/te-benchmark/config/sdormpostgresql.xml

rm -f ${IROOT}/odbcinst.ini
rm -f ${IROOT}/odbc.ini

cp ${IROOT}/ffead-cpp-2.0/resources/sample-odbcinst.ini ${IROOT}/odbcinst.ini
cp ${IROOT}/ffead-cpp-2.0/resources/sample-odbc.ini ${IROOT}/odbc.ini

sed -i 's|localhost|'${DBHOST}'|g' ${IROOT}/odbc.ini

fw_get -o mongo-c-driver-1.4.0.tar.gz https://github.com/mongodb/mongo-c-driver/releases/download/1.4.0/mongo-c-driver-1.4.0.tar.gz
fw_untar mongo-c-driver-1.4.0.tar.gz
cd mongo-c-driver-1.4.0/
./configure --prefix=${IROOT} --libdir=${IROOT} --disable-automatic-init-and-cleanup
make && make install
cd -

touch ${IROOT}/ffead-cpp.installed

