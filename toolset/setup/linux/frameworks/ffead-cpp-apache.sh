#!/bin/bash

fw_installed ffead-cpp-apache && return 0

fw_get -o unixODBC-2.3.4.tar.gz ftp://ftp.unixodbc.org/pub/unixODBC/unixODBC-2.3.4.tar.gz
fw_untar unixODBC-2.3.4.tar.gz
cd unixODBC-2.3.4
./configure --enable-stats=no --enable-gui=no --enable-drivers=no --enable-iconv --with-iconv-char-enc=UTF8 --with-iconv-ucode-enc=UTF16LE --libdir=${IROOT} --prefix=${IROOT} --sysconfdir=${IROOT}
make install
cd -

fw_get -o mongo-c-driver-1.4.0.tar.gz https://github.com/mongodb/mongo-c-driver/releases/download/1.4.0/mongo-c-driver-1.4.0.tar.gz
fw_untar mongo-c-driver-1.4.0.tar.gz
cd mongo-c-driver-1.4.0/
./configure --prefix=${IROOT} --libdir=${IROOT} --disable-automatic-init-and-cleanup
make && make install
cd -

sudo apt-get install -y build-essential
sudo apt-get install -y uuid-dev libmyodbc odbc-postgresql

if [ ! -d "${IROOT}/ffead-cpp-2.0" ]; then
	fw_get -o ffead-cpp-2.0.tar.gz https://github.com/sumeetchhetri/ffead-cpp/releases/download/2.0/ffead-cpp-2.0-te-bin.tar.gz
	fw_untar ffead-cpp-2.0.tar.gz

	rm -rf ffead-cpp-2.0
	cp -rf ffead-cpp-2.0-bin ${IROOT}/ffead-cpp-2.0
fi

fw_get -o httpd-2.4.25.tar.gz http://www-us.apache.org/dist//httpd/httpd-2.4.25.tar.gz
fw_get -o apr-1.5.2.tar.gz http://www-us.apache.org/dist//apr/apr-1.5.2.tar.gz
fw_get -o apr-util-1.5.4.tar.gz http://www-us.apache.org/dist//apr/apr-util-1.5.4.tar.gz
fw_untar httpd-2.4.25.tar.gz
fw_untar apr-1.5.2.tar.gz
fw_untar apr-util-1.5.4.tar.gz
mv -f apr-1.5.2 httpd-2.4.25/srclib/apr
mv -f apr-util-1.5.4 httpd-2.4.25/srclib/apr-util
cd ${IROOT}/httpd-2.4.25
rm -rf ${IROOT}/httpd
mkdir ${IROOT}/httpd
./configure --prefix=${IROOT}/httpd --enable-mods-shared=all --with-included-apr
make
make install
cd -

fw_get -o mod_ffeadcpp.cpp https://raw.githubusercontent.com/sumeetchhetri/ffead-cpp/master/modules/apache_mod_ffeadcpp/mod_ffeadcpp.cpp
g++ -fpic -DSHARED_MODULE -fpermissive -I"${IROOT}/httpd/include" -I"${IROOT}/ffead-cpp-2.0/include/" -I"${IROOT}/include" -I"${IROOT}/include/libbson-1.0/" -I"${IROOT}/include/libmongoc-1.0" mod_ffeadcpp.cpp -L"${IROOT}/ffead-cpp-2.0/lib" -L"${IROOT}" -lffead_common -lffead_framework -ldl -lcrypto -lssl -c mod_ffeadcpp.cpp
g++ -shared -o mod_ffeadcpp.so mod_ffeadcpp.o -L"${IROOT}/ffead-cpp-2.0/lib" -L"${IROOT}" -L"${IROOT}/httpd/lib" -lffead_common -lffead_framework -ldl -lcrypto -lssl -lapr-1 -laprutil-1 -lstdc++
${IROOT}/httpd/bin/apxs -i -n 'ffead_cpp' mod_ffeadcpp.so

sed -i 's|localhost|'${DBHOST}'|g' ${IROOT}/ffead-cpp-2.0/web/te-benchmark/config/sdorm.xml
sed -i 's|localhost|'${DBHOST}'|g' ${IROOT}/ffead-cpp-2.0/web/te-benchmark/config/sdormmongo.xml
sed -i 's|localhost|'${DBHOST}'|g' ${IROOT}/ffead-cpp-2.0/web/te-benchmark/config/sdormmysql.xml
sed -i 's|localhost|'${DBHOST}'|g' ${IROOT}/ffead-cpp-2.0/web/te-benchmark/config/sdormpostgresql.xml

cp ${IROOT}/ffead-cpp-2.0/resources/sample-odbcinst.ini ${IROOT}/odbcinst.ini
cp ${IROOT}/ffead-cpp-2.0/resources/sample-odbc.ini ${IROOT}/odbc.ini

sed -i 's|localhost|'${DBHOST}'|g' ${IROOT}/odbc.ini

cp -rf ${IROOT}/ffead-cpp-2.0 ${IROOT}/httpd/htdocs/

FFEADROOT=${IROOT}/httpd/htdocs/ffead-cpp-2.0
ETROOT=${FFEADROOT//\//\\/}
EIROOT=${IROOT//\//\\/}

sed -i 's/Listen 80.*/#Listen 80/' ${IROOT}/httpd/conf/httpd.conf
sed -i 's/Include conf\/ffead-site\.conf//' ${IROOT}/httpd/conf/httpd.conf
echo 'Include conf/ffead-site.conf' >> ${IROOT}/httpd/conf/httpd.conf

if [ ! -f ${IROOT}/httpd/bin/envvars  ]; then
bash -c 'cat > ${IROOT}/httpd/bin/envvars <<EOL
export FFEAD_CPP_PATH='"${FFEADROOT}"'
export LD_LIBRARY_PATH=\$FFEAD_CPP_PATH/lib:'"${IROOT}"':\$LD_LIBRARY_PATH/
export ODBCINI='"$IROOT"'/odbc.ini
export ODBCSYSINI='"$IROOT"'	
EOL'
else
	if [ ! -f ${IROOT}/httpd/bin/envvars.bk  ]; then
		cp ${IROOT}/httpd/bin/envvars ${IROOT}/httpd/bin/envvars.bk
	fi
	cp ${IROOT}/httpd/bin/envvars.bk ${IROOT}/httpd/bin/envvars
	sed -i '/^export FFEAD_CPP_PATH/ d' ${IROOT}/httpd/bin/envvars
	sed -i '/^export LD_LIBRARY_PATH/ d' ${IROOT}/httpd/bin/envvars
	sed -i '/^export ODBCINI/ d' ${IROOT}/httpd/bin/envvars
	sed -i '/^export ODBCSYSINI/ d' ${IROOT}/httpd/bin/envvars	
bash -c 'cat <<EOL >> ${IROOT}/httpd/bin/envvars
export FFEAD_CPP_PATH='"${FFEADROOT}"'
export LD_LIBRARY_PATH='"${FFEADROOT}"'/lib:'"${IROOT}"':$LD_LIBRARY_PATH/
export ODBCINI='"$IROOT"'/odbc.ini
export ODBCSYSINI='"$IROOT"' 
EOL'
fi

bash -c 'rm -f ${IROOT}/httpd/conf/ffead-site.conf'

bash -c 'cat > ${IROOT}/httpd/conf/ffead-site.conf <<EOL
LoadModule ffead_cpp_module '"${IROOT}"'/httpd/modules/mod_ffeadcpp.so
Listen 8080
FFEAD_CPP_PATH '"${FFEADROOT}"'
<VirtualHost *:8080>
	DocumentRoot '"${FFEADROOT}"'/web
	SetHandler ffead_cpp_module
	<Directory '"${FFEADROOT}"'>
		Options FollowSymLinks
		AllowOverride None
		Require all denied
	</Directory>
	<Directory '"${FFEADROOT}"'/web/>
		Options -Indexes +FollowSymLinks +MultiViews
		AllowOverride All
		Require all granted
	</Directory>
</VirtualHost>
EOL'

touch ${IROOT}/ffead-cpp-apache.installed
