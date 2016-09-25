#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/ffead-cpp-apache.installed)
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

sudo rm -rf /var/www/ffead-cpp-2.0
sudo cp -R ffead-cpp-2.0-bin/ /var/www
sudo mv /var/www/ffead-cpp-2.0-bin /var/www/ffead-cpp-2.0
rm -rf ffead-cpp-2.0/

sudo sed -i 's|localhost|'${DBHOST}'|g' ${TROOT}/ffead-cpp-2.0/web/te-benchmark/config/sdorm*

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

FFEADROOT=/var/www/ffead-cpp-2.0
ETROOT=${FFEADROOT//\//\\/}
EIROOT=${IROOT//\//\\/}

sudo sed -i 's/.*Listen 80.*/#Listen 80/' /etc/apache2/ports.conf
sudo sed -i '/^export FFEAD_CPP_PATH=/{h;s/=.*/='"${ETROOT}"'/};${x;/^$/{s//export FFEAD_CPP_PATH='"${ETROOT}"'/;H};x}' /etc/apache2/envvars
sudo sed -i '/^export LD_LIBRARY_PATH=/{h;s/=.*/='"${EIROOT}"':$FFEAD_CPP_PATH\/lib:$LD_LIBRARY_PATH/};${x;/^$/{s//export LD_LIBRARY_PATH='"${EIROOT}"':$FFEAD_CPP_PATH\/lib:$LD_LIBRARY_PATH/;H};x}' /etc/apache2/envvars

sudo bash -c 'rm -f /etc/apache2/sites-enabled/ffead-site.conf'

sudo bash -c 'cat > /etc/apache2/sites-enabled/ffead-site.conf <<EOL
LoadModule ffead_cpp_module '"${FFEADROOT}"'/mod_ffeadcpplib.so
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

sudo chown -R www-data:www-data ${FFEADROOT}
sudo chmod -R g+rw ${FFEADROOT}

touch ${IROOT}/ffead-cpp-apache.installed
