#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/ffead-cpp-nginx.installed)
[ ! "$RETCODE" == 0 ] || { return 0; }

fw_get -o unixODBC-2.3.4.tar.gz ftp://ftp.unixodbc.org/pub/unixODBC/unixODBC-2.3.4.tar.gz
fw_untar unixODBC-2.3.4.tar.gz
cd unixODBC-2.3.4
./configure --enable-stats=no --enable-gui=no --enable-drivers=no --enable-iconv --with-iconv-char-enc=UTF8 --with-iconv-ucode-enc=UTF16LE --libdir=/usr/lib/x86_64-linux-gnu --prefix=/usr --sysconfdir=/etc
sudo make install

sudo apt-get install build-essential
sudo apt-get install -y uuid-dev libmyodbc odbc-postgresql

fw_get -o ffead-cpp-2.0.tar.gz https://github.com/sumeetchhetri/ffead-cpp/releases/download/2.0/ffead-cpp-2.0-te-bin.tar.gz
fw_untar ffead-cpp-2.0.tar.gz

sudo rm -rf ${TROOT}/ffead-cpp-2.0
cp -R ffead-cpp-2.0-bin/ ${TROOT}
mv ${TROOT}/ffead-cpp-2.0-bin ${TROOT}/ffead-cpp-2.0
rm -rf ffead-cpp-2.0/

fw_get -o nginx-1.11.3.tar.gz http://nginx.org/download/nginx-1.11.3.tar.gz
fw_untar nginx-1.11.3.tar.gz
sudo rm -rf ${IROOT}/nginxfc
cd nginx-1.11.3
./configure --prefix=${IROOT}/nginxfc --with-ld-opt="-lstdc++ -L${TROOT}/ffead-cpp-2.0/lib -L${IROOT}" --add-module="${TROOT}/ffead-cpp-2.0/ngx_mod" --with-cc-opt="-I${IROOT}/include/libmongoc-1.0/ -I${IROOT}/include/libbson-1.0/ -I${TROOT}/ffead-cpp-2.0/include -w -fpermissive"
make install

sed -i 's|localhost|'${DBHOST}'|g' ${TROOT}/ffead-cpp-2.0/web/te-benchmark/config/sdorm*

sudo rm -f /etc/odbcinst.ini
sudo rm -f /etc/odbc.ini

sudo cp ${TROOT}/ffead-cpp-2.0/resources/sample-odbcinst.ini /etc/odbcinst.ini
sudo cp ${TROOT}/ffead-cpp-2.0/resources/sample-odbc.ini /etc/odbc.ini

sudo sed -i 's|localhost|'${DBHOST}'|g' /etc/odbc.ini

fw_get -o mongo-c-driver-1.4.0.tar.gz https://github.com/mongodb/mongo-c-driver/releases/download/1.4.0/mongo-c-driver-1.4.0.tar.gz
fw_untar mongo-c-driver-1.4.0.tar.gz
cd mongo-c-driver-1.4.0/
./configure --prefix=${IROOT} --libdir=${IROOT} --disable-automatic-init-and-cleanup
make && sudo make install

cp ${TROOT}/ffead-cpp-2.0/ngx_mod/nginx.conf ${IROOT}/nginxfc/conf/
sed -i 's|FFEAD_PATH|'${TROOT}/ffead-cpp-2.0'|g' ${IROOT}/nginxfc/conf/nginx.conf

touch ${IROOT}/ffead-cpp-nginx.installed
