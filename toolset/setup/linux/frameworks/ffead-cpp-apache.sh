#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/ffead-cpp-apache.installed)
[ ! "$RETCODE" == 0 ] || { return 0; }

sudo apt-get install -y uuid-dev unixodbc unixodbc-dev

fw_get -o ffead-cpp-2.0.tar.gz https://github.com/sumeetchhetri/ffead-cpp/releases/download/v2.0-Draft-TLV-Fixed-TE_Benchmark/ffead-cpp-2.0-tlfixed-bin.tar.gz
fw_untar ffead-cpp-2.0.tar.gz

cp -R ffead-cpp-2.0-bin/ ${TROOT}/ffead-cpp-2.0
rm -rf ffead-cpp-2.0/

wget https://github.com/mongodb/mongo-c-driver/releases/download/1.1.10/mongo-c-driver-1.1.10.tar.gz
tar -xzf mongo-c-driver-1.1.10.tar.gz
cd mongo-c-driver-1.1.10/
./configure --prefix=${IROOT} --libdir=${IROOT}
make && sudo make install

FFEADROOT=$TROOT/ffead-cpp-2.0
ETROOT=${FFEADROOT//\//\\/}
EIROOT=${IROOT//\//\\/}

sudo sed -i '/^export FFEAD_CPP_PATH=/{h;s/=.*/='"${ETROOT}"'/};${x;/^$/{s//export FFEAD_CPP_PATH='"${ETROOT}"'/;H};x}' /etc/apache2/envvars
sudo sed -i '/^export LD_LIBRARY_PATH=/{h;s/=.*/='"${EIROOT}"':$FFEAD_CPP_PATH\/lib:$LD_LIBRARY_PATH/};${x;/^$/{s//export LD_LIBRARY_PATH='"${EIROOT}"':$FFEAD_CPP_PATH\/lib:$LD_LIBRARY_PATH/;H};x}' /etc/apache2/envvars

sudo bash -c 'rm -f /etc/apache2/sites-enabled/ffead-site.conf'

sudo bash -c 'cat > /etc/apache2/sites-enabled/ffead-site.conf <<EOL
LoadModule ffead_cpp_module '"${FFEADROOT}"'/mod_ffeadcpplib.so
Listen 8080
FFEAD_CPP_PATH '"${TROOT}"'/ffead-cpp-2.0

<VirtualHost *:8080>
        DocumentRoot '"${TROOT}"'/ffead-cpp-2.0/web
        SetHandler ffead_cpp_module
</VirtualHost>

<Directory '"${TROOT}"'/ffead-cpp-2.0/web/>
        Options Indexes FollowSymLinks
        AllowOverride None
        Require all granted
</Directory>
EOL'
#sudo bash -c 'echo -e "LoadModule ffead_cpp_module '"${FFEADROOT}"'/mod_ffeadcpplib.so" >> /etc/apache2/sites-enabled/ffead-site.conf'
#sudo bash -c 'echo -e "Listen 8080" >> /etc/apache2/sites-enabled/ffead-site.conf'
#sudo bash -c 'echo -e "FFEAD_CPP_PATH '"${TROOT}"'/ffead-cpp-2.0\n" >> /etc/apache2/sites-enabled/ffead-site.conf'
#sudo bash -c 'echo -e "<VirtualHost *:8080>" >> /etc/apache2/sites-enabled/ffead-site.conf'
#sudo bash -c 'echo -e "\tDocumentRoot '"${TROOT}"'/ffead-cpp-2.0/web" >> /etc/apache2/sites-enabled/ffead-site.conf'
#sudo bash -c 'echo -e "\tSetHandler ffead_cpp_module" >> /etc/apache2/sites-enabled/ffead-site.conf'
#sudo bash -c 'echo -e "</VirtualHost>" >> /etc/apache2/sites-enabled/ffead-site.conf'

touch ${IROOT}/ffead-cpp-apache.installed

