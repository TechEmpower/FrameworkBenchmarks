#!/bin/bash

cd $IROOT

wget -q http://nginx.org/download/nginx-1.13.1.tar.gz
tar xf nginx-1.13.1.tar.gz

cd $IROOT/nginx-1.13.1

./configure \
    --prefix=${IROOT}/nginxfc \
    --with-ld-opt="-lstdc++ -lhiredis -lmemcachedutil -L${IROOT}/ffead-cpp-2.0/lib -L${IROOT} -L${IROOT}/lib" \
    --add-module="${IROOT}/ffead-cpp-src/modules/nginx_mod_ffeadcpp" \
    --with-cc-opt="-I${IROOT}/ffead-cpp-2.0/include -I${IROOT}/include -I${IROOT}/include/libmongoc-1.0 -I${IROOT}/include/libbson-1.0 -w -fpermissive -std=gnu++11"
make
make install

cp ${IROOT}/ffead-cpp-src/modules/nginx_mod_ffeadcpp/nginx.conf ${IROOT}/nginxfc/conf/
sed -i 's|FFEAD_PATH|'${IROOT}/ffead-cpp-2.0'|g' ${IROOT}/nginxfc/conf/nginx.conf