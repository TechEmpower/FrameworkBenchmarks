#!/bin/bash

fw_depends cutelyst nginx

sed -i 's|DatabaseHostName=.*|DatabaseHostName='"$DBHOST"'|g' config/config_socket.ini
sed -i 's|SendDate=.*|SendDate=false|g' config/config_socket.ini
sed -i 's|include .*/conf/uwsgi_params;|include '"${NGINX_HOME}"'/conf/uwsgi_params;|g' nginx.conf

cd $IROOT
mkdir cutelyst-benchmarks || true
cd cutelyst-benchmarks
rm -rf *

QT_VERSION_MM=56
export CMAKE_PREFIX_PATH=/opt/qt${QT_VERSION_MM}:${IROOT}

cmake $TROOT -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=$IROOT

make clean
make -j $MAX_THREADS

nginx -c $TROOT/nginx.conf

export LD_LIBRARY_PATH=/opt/qt${QT_VERSION_MM}/lib:${IROOT}/lib/x86_64-linux-gnu/
uwsgi --ini ${TROOT}/config/config_socket.ini --cutelyst-app ${IROOT}/cutelyst-benchmarks/src/libcutelyst_benchmarks.so -p $MAX_THREADS &
