#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/ffead-cpp-apache.installed)
[ ! "$RETCODE" == 0 ] || { return 0; }

sudo apt-get install -y uuid-dev unixodbc unixodbc-dev

fw_get -o ffead-cpp-2.0.tar.gz https://github.com/sumeetchhetri/ffead-cpp/releases/download/v2.0-Draft-TLV-Fixed-TE_Benchmark/ffead-cpp-2.0-tlfixed-bin.tar.gz
fw_untar ffead-cpp-2.0.tar.gz

rm -rf ${TROOT}/ffead-cpp-2.0-bin
cp -R ffead-cpp-2.0-bin/ ${TROOT}
mv ${TROOT}/ffead-cpp-2.0-bin ${TROOT}/ffead-cpp-2.0
rm -rf ffead-cpp-2.0/

wget https://github.com/mongodb/mongo-c-driver/releases/download/1.1.10/mongo-c-driver-1.1.10.tar.gz
tar -xzf mongo-c-driver-1.1.10.tar.gz
cd mongo-c-driver-1.1.10/
./configure --prefix=${IROOT} --libdir=${IROOT}
make && sudo make install

FFEADROOT=$TROOT/ffead-cpp-2.0
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
</VirtualHost>
<Directory '"${FFEADROOT}"'/web/>
       	Options Indexes FollowSymLinks MultiViews
       	AllowOverride None
       	Require all granted
</Directory>
EOL'

sudo adduser testrunner www-data
sudo chown -R www-data:www-data ${FFEADROOT}
sudo chmod -R g+rw ${FFEADROOT}

touch ${IROOT}/ffead-cpp-apache.installed
