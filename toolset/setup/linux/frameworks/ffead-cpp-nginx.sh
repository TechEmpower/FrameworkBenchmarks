#!/bin/bash

fw_installed ffead-cpp-nginx && return 0

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

fw_get -o mongo-c-driver-1.4.0.tar.gz https://github.com/mongodb/mongo-c-driver/releases/download/1.4.0/mongo-c-driver-1.4.0.tar.gz
fw_untar mongo-c-driver-1.4.0.tar.gz
cd mongo-c-driver-1.4.0/
./configure --prefix=${IROOT} --libdir=${IROOT} --disable-automatic-init-and-cleanup
make && make install
cd -

fw_get -o nginx-1.11.3.tar.gz http://nginx.org/download/nginx-1.11.3.tar.gz
fw_untar nginx-1.11.3.tar.gz
rm -rf ${IROOT}/nginxfc
cd nginx-1.11.3
./configure --prefix=${IROOT}/nginxfc --with-ld-opt="-lstdc++ -L${IROOT}/ffead-cpp-2.0/lib -L${IROOT}" --add-module="${IROOT}/ffead-cpp-2.0/ngx_mod" --with-cc-opt="-I${IROOT}/include/ -I${IROOT}/include/libmongoc-1.0/ -I${IROOT}/include/libbson-1.0/ -I${IROOT}/ffead-cpp-2.0/include -w -fpermissive"
make install
cd -

sed -i 's|localhost|'${DBHOST}'|g' ${IROOT}/ffead-cpp-2.0/web/te-benchmark/config/sdorm.xml
sed -i 's|localhost|'${DBHOST}'|g' ${IROOT}/ffead-cpp-2.0/web/te-benchmark/config/sdormmongo.xml
sed -i 's|localhost|'${DBHOST}'|g' ${IROOT}/ffead-cpp-2.0/web/te-benchmark/config/sdormmysql.xml
sed -i 's|localhost|'${DBHOST}'|g' ${IROOT}/ffead-cpp-2.0/web/te-benchmark/config/sdormpostgresql.xml

rm -f ${IROOT}/odbcinst.ini
rm -f ${IROOT}/odbc.ini

cp ${IROOT}/ffead-cpp-2.0/resources/sample-odbcinst.ini ${IROOT}/odbcinst.ini
cp ${IROOT}/ffead-cpp-2.0/resources/sample-odbc.ini ${IROOT}/odbc.ini

sed -i 's|localhost|'${DBHOST}'|g' ${IROOT}/odbc.ini

cp ${IROOT}/ffead-cpp-2.0/ngx_mod/nginx.conf ${IROOT}/nginxfc/conf/
sed -i 's|FFEAD_PATH|'${IROOT}/ffead-cpp-2.0'|g' ${IROOT}/nginxfc/conf/nginx.conf
echo "env ODBCINI=${IROOT}/odbc.ini;" | cat - ${IROOT}/nginxfc/conf/nginx.conf > /tmp/out && mv /tmp/out ${IROOT}/nginxfc/conf/nginx.conf
echo "env ODBCSYSINI=${IROOT};" | cat - ${IROOT}/nginxfc/conf/nginx.conf > /tmp/out && mv /tmp/out ${IROOT}/nginxfc/conf/nginx.conf

touch ${IROOT}/ffead-cpp-nginx.installed
