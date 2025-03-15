#!/bin/bash

apt update -yqq && apt install --no-install-recommends -yqq autoconf-archive unzip uuid-dev odbc-postgresql unixodbc unixodbc-dev \
	apache2 apache2-dev libapr1-dev libaprutil1-dev memcached libmemcached-dev redis-server libssl-dev \
	zlib1g-dev cmake make clang-format ninja-build libcurl4-openssl-dev git libpq-dev libpugixml-dev\
	wget build-essential pkg-config libpcre3-dev curl libgtk2.0-dev libgdk-pixbuf2.0-dev bison flex libreadline-dev 
apt-get install --reinstall ca-certificates

cd $IROOT

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
git checkout ea8c36c65bf9cf83aaf6b0db971248c6ae3686cf -b works
cmake -DCMAKE_INSTALL_PREFIX=/usr .
make install
cd $IROOT
rm -rf libcuckoo

wget -q https://mirror.csclub.uwaterloo.ca/mysql/Downloads/Connector-ODBC/8.0/mysql-connector-odbc_8.0.29-1ubuntu21.10_amd64.deb
dpkg -i mysql-connector-odbc_8.0.29-1ubuntu21.10_amd64.deb
wget -q https://mirror.csclub.uwaterloo.ca/mysql/Downloads/Connector-ODBC/8.0/mysql-connector-odbc-setup_8.0.29-1ubuntu21.10_amd64.deb
dpkg -i mysql-connector-odbc-setup_8.0.29-1ubuntu21.10_amd64.deb
rm -f *.deb

#wget -q https://github.com/mongodb/mongo-c-driver/releases/download/1.4.2/mongo-c-driver-1.4.2.tar.gz
#tar xf mongo-c-driver-1.4.2.tar.gz
#rm -f mongo-c-driver-1.4.2.tar.gz
#cd mongo-c-driver-1.4.2/ && \
#    ./configure --disable-automatic-init-and-cleanup && \
#    make && make install
#cd $IROOT
#rm -rf mongo-c-driver-1.4.2
MONGO_VERSION=1.26.2
wget "https://github.com/mongodb/mongo-c-driver/archive/refs/tags/$MONGO_VERSION.tar.gz" --output-document="mongo-c-driver-$MONGO_VERSION.tar.gz"
tar xf "mongo-c-driver-$MONGO_VERSION.tar.gz"
rm -f "mongo-c-driver-$MONGO_VERSION.tar.gz"
cd mongo-c-driver-$MONGO_VERSION/ && mkdir _build && cmake -S . -B _build \
-D ENABLE_EXTRA_ALIGNMENT=OFF \
-D ENABLE_AUTOMATIC_INIT_AND_CLEANUP=OFF -D ENABLE_TESTS=OFF -D ENABLE_EXAMPLES=OFF \
-D CMAKE_BUILD_TYPE=RelWithDebInfo \
-D BUILD_VERSION="$MONGO_VERSION" \
-D ENABLE_SSL=OFF \
-D ENABLE_SASL=OFF \
-D ENABLE_MONGOC=ON && cmake --build _build --config RelWithDebInfo --parallel && cmake --install _build
cd $IROOT
rm -rf "mongo-c-driver-$MONGO_VERSION"

wget -q https://github.com/redis/hiredis/archive/v1.0.2.tar.gz
tar xf v1.0.2.tar.gz
rm -f v1.0.2.tar.gz
cd hiredis-1.0.2/
cmake . && make install
cd $IROOT
rm -rf hiredis-1.0.2

wget -q https://github.com/sewenew/redis-plus-plus/archive/refs/tags/1.3.10.tar.gz
tar xf 1.3.10.tar.gz
rm -f 1.3.10.tar.gz
cd redis-plus-plus-1.3.10/
mkdir build
cd build
cmake -DREDIS_PLUS_PLUS_CXX_STANDARD=17 .. && make && make install
cd $IROOT
rm -rf redis-plus-plus-1.3.10

cd $IROOT
wget -q https://github.com/microsoft/mimalloc/archive/v2.1.2.tar.gz
tar xf v2.1.2.tar.gz
rm -f v2.1.2.tar.gz
cd mimalloc-2.1.2
mkdir -p out/release
cd out/release
cmake ../.. -DCMAKE_BUILD_TYPE=Release
make && make install
cd $IROOT
rm -rf mimalloc-2.1.2

wget -q https://github.com/microsoft/snmalloc/archive/0.6.2.tar.gz
tar xf 0.6.2.tar.gz
rm -f 0.6.2.tar.gz
cd snmalloc-0.6.2
mkdir build
cd build
cmake -G Ninja .. -DCMAKE_BUILD_TYPE=Release
ninja
cd $IROOT
rm -rf snmalloc-0.6.2
