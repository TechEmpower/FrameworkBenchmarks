#!/bin/bash

apt update -yqq && apt install --no-install-recommends -yqq autoconf-archive unzip uuid-dev odbc-postgresql unixodbc unixodbc-dev \
	apache2 apache2-dev libapr1-dev libaprutil1-dev memcached libmemcached-dev redis-server libssl-dev \
	zlib1g-dev cmake make clang-format-9 ninja-build libcurl4-openssl-dev libpq-dev git \
	wget build-essential pkg-config libpcre3-dev curl libgtk2.0-dev libgdk-pixbuf2.0-dev
apt-get install --reinstall ca-certificates

mkdir /usr/local/share/ca-certificates/cacert.org
wget -P /usr/local/share/ca-certificates/cacert.org http://www.cacert.org/certs/root.crt http://www.cacert.org/certs/class3.crt
update-ca-certificates
git config --global http.sslCAinfo /etc/ssl/certs/ca-certificates.crt

#redis will not start correctly on bionic with this config
sed -i "s/bind .*/bind 127.0.0.1/g" /etc/redis/redis.conf

echo never > /sys/kernel/mm/transparent_hugepage/enabled
echo 'echo never > /sys/kernel/mm/transparent_hugepage/enabled' >> /etc/rc.local
sysctl vm.overcommit_memory=1

service apache2 stop
service memcached stop
service redis-server stop

cd $IROOT
git clone https://github.com/efficient/libcuckoo.git
cd libcuckoo
git checkout 8785773896d74f72b6224e59d37f5f8c3c1e022a -b works
cmake -DCMAKE_INSTALL_PREFIX=/usr .
make install
cd $IROOT
rm -rf libcuckoo

wget -q https://downloads.mysql.com/archives/get/p/10/file/mysql-connector-odbc_8.0.21-1ubuntu20.04_amd64.deb
dpkg -i mysql-connector-odbc_8.0.21-1ubuntu20.04_amd64.deb
wget -q https://downloads.mysql.com/archives/get/p/10/file/mysql-connector-odbc-setup_8.0.21-1ubuntu20.04_amd64.deb
dpkg -i mysql-connector-odbc-setup_8.0.21-1ubuntu20.04_amd64.deb
rm -f *.deb

wget -q https://github.com/mongodb/mongo-c-driver/releases/download/1.4.2/mongo-c-driver-1.4.2.tar.gz
tar xf mongo-c-driver-1.4.2.tar.gz
rm -f mongo-c-driver-1.4.2.tar.gz
cd mongo-c-driver-1.4.2/ && \
    ./configure --disable-automatic-init-and-cleanup && \
    make && make install
cd $IROOT
rm -rf mongo-c-driver-1.4.2 

wget -q https://github.com/redis/hiredis/archive/v1.0.0.tar.gz
tar xf v1.0.0.tar.gz
rm -f v1.0.0.tar.gz
cd hiredis-1.0.0/
cmake . && make install
cd $IROOT
rm -rf hiredis-1.0.0

cd $IROOT
wget -q https://github.com/microsoft/mimalloc/archive/v1.6.3.tar.gz
tar xf v1.6.3.tar.gz
rm -f v1.6.3.tar.gz
cd mimalloc-1.6.3
mkdir -p out/release
cd out/release
cmake ../.. -DCMAKE_BUILD_TYPE=Release
make && make install
cd $IROOT
rm -rf mimalloc-1.6.3

wget -q https://github.com/microsoft/snmalloc/archive/0.4.2.tar.gz
tar xf 0.4.2.tar.gz
rm -f 0.4.2.tar.gz
cd snmalloc-0.4.2
mkdir build
cd build
cmake -G Ninja .. -DCMAKE_BUILD_TYPE=Release
ninja
cd $IROOT
