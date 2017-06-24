#!/bin/bash

fw_installed ffead-cpp-httpd && return 0

fw_depends ffead-cpp-framework

sudo apt-get remove -y apache2

fw_get -o httpd-2.4.25.tar.gz http://www-us.apache.org/dist//httpd/httpd-2.4.25.tar.gz
fw_get -o apr-1.5.2.tar.gz http://www-us.apache.org/dist//apr/apr-1.5.2.tar.gz
fw_get -o apr-util-1.5.4.tar.gz http://www-us.apache.org/dist//apr/apr-util-1.5.4.tar.gz
rm -rf ${IROOT}/httpd-2.4.25
rm -rf ${IROOT}/apr-1.5.2
rm -rf ${IROOT}/apr-util-1.5.4
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
cd ${IROOT}

cd ${IROOT}/ffead-cpp-src/modules/apache_mod_ffeadcpp/
g++ -fpic -DSHARED_MODULE -fpermissive -I"${IROOT}/httpd/include" -I"${IROOT}/ffead-cpp-2.0/include/" -I"${IROOT}/include" -I"${IROOT}/include/libbson-1.0/" -I"${IROOT}/include/libmongoc-1.0" mod_ffeadcpp.cpp -L"${IROOT}/ffead-cpp-2.0/lib" -L"${IROOT}" -lffead_common -lffead_framework -ldl -lcrypto -lssl -c mod_ffeadcpp.cpp
g++ -shared -o mod_ffeadcpp.so mod_ffeadcpp.o -L"${IROOT}/ffead-cpp-2.0/lib" -L"${IROOT}" -L"${IROOT}/httpd/lib" -lffead_common -lffead_framework -ldl -lcrypto -lssl -lapr-1 -laprutil-1 -lstdc++
${IROOT}/httpd/bin/apxs -i -n 'ffead_cpp' mod_ffeadcpp.so
cd -

FFEADROOT=${IROOT}/ffead-cpp-2.0
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

touch ${IROOT}/ffead-cpp-httpd.installed