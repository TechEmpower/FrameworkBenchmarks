#!/bin/bash

fw_installed ffead-cpp-nginx && return 0

fw_depends ffead-cpp-framework

sudo apt-get remove -y nginx

fw_get -o nginx-1.11.3.tar.gz http://nginx.org/download/nginx-1.11.3.tar.gz
rm -rf ${IROOT}nginx-1.11.3
fw_untar nginx-1.11.3.tar.gz
rm -rf ${IROOT}/nginxfc
cd nginx-1.11.3
./configure --prefix=${IROOT}/nginxfc --with-ld-opt="-lstdc++ -L${IROOT}/ffead-cpp-2.0/lib -L${IROOT}" --add-module="${IROOT}/ffead-cpp-src/modules/nginx_mod_ffeadcpp" --with-cc-opt="-I${IROOT}/include -I${IROOT}/include/libmongoc-1.0/ -I${IROOT}/include/libbson-1.0/ -I${IROOT}/ffead-cpp-2.0/include -w -fpermissive"
make install

cp ${IROOT}/ffead-cpp-src/modules/nginx_mod_ffeadcpp/nginx.conf ${IROOT}/nginxfc/conf/
sed -i 's|FFEAD_PATH|'${IROOT}/ffead-cpp-2.0'|g' ${IROOT}/nginxfc/conf/nginx.conf

touch ${IROOT}/ffead-cpp-nginx.installed
