#!/bin/bash

fw_installed ffead-cpp && return 0

sudo apt-get remove -y libodbc1 unixodbc unixodbc-dev

if [ ! -f "${IROOT}/ffead-cpp-unixodbc.installed" ]; then
	fw_get -o unixODBC-2.3.4.tar.gz ftp://ftp.unixodbc.org/pub/unixODBC/unixODBC-2.3.4.tar.gz
	fw_untar unixODBC-2.3.4.tar.gz
	cd unixODBC-2.3.4
	./configure --enable-stats=no --enable-gui=no --enable-drivers=no --enable-iconv --with-iconv-char-enc=UTF8 --with-iconv-ucode-enc=UTF16LE --libdir=${IROOT} --prefix=${IROOT} --sysconfdir=${IROOT}
	sudo make install
	cd -
	touch ${IROOT}/ffead-cpp-unixodbc.installed
fi

sudo apt-get install -y build-essential
sudo apt-get install -y uuid-dev libmyodbc odbc-postgresql

fw_get -o ffead-cpp-2.0.tar.gz https://github.com/sumeetchhetri/ffead-cpp/releases/download/2.0/ffead-cpp-2.0-te-bin.tar.gz
fw_untar ffead-cpp-2.0.tar.gz

sudo rm -rf ${IROOT}/ffead-cpp-2.0
#cp -R ffead-cpp-2.0-bin/ ${IROOT}
mv ${IROOT}/ffead-cpp-2.0-bin ${IROOT}/ffead-cpp-2.0
#rm -rf ffead-cpp-2.0/

sudo sed -i 's|localhost|'${DBHOST}'|g' ${IROOT}/ffead-cpp-2.0/web/te-benchmark/config/sdorm.xml
sudo sed -i 's|localhost|'${DBHOST}'|g' ${IROOT}/ffead-cpp-2.0/web/te-benchmark/config/sdormmongo.xml
sudo sed -i 's|localhost|'${DBHOST}'|g' ${IROOT}/ffead-cpp-2.0/web/te-benchmark/config/sdormmysql.xml
sudo sed -i 's|localhost|'${DBHOST}'|g' ${IROOT}/ffead-cpp-2.0/web/te-benchmark/config/sdormpostgresql.xml

sudo rm -f /etc/odbcinst.ini
sudo rm -f /etc/odbc.ini

sudo cp ${IROOT}/ffead-cpp-2.0/resources/sample-odbcinst.ini ${IROOT}/odbcinst.ini
sudo cp ${IROOT}/ffead-cpp-2.0/resources/sample-odbc.ini ${IROOT}/odbc.ini

sudo sed -i 's|localhost|'${DBHOST}'|g' ${IROOT}/odbc.ini

if [ ! -f "${IROOT}/ffead-cpp-mongocdriver.installed" ]; then
	fw_get -o mongo-c-driver-1.4.0.tar.gz https://github.com/mongodb/mongo-c-driver/releases/download/1.4.0/mongo-c-driver-1.4.0.tar.gz
	fw_untar mongo-c-driver-1.4.0.tar.gz
	cd mongo-c-driver-1.4.0/
	./configure --prefix=${IROOT} --libdir=${IROOT} --disable-automatic-init-and-cleanup
	make && sudo make install
	cd -
	touch ${IROOT}/ffead-cpp-mongocdriver.installed
fi

touch ${IROOT}/ffead-cpp.installed

