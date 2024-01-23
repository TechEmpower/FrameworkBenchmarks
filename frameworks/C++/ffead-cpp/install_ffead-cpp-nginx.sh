#!/bin/bash

cd $IROOT

cp ${IROOT}/ffead-cpp-src/modules/nginx_mod_ffeadcpp/nginx.conf ${IROOT}/nginx-ffead-mongo/conf/
sed -i 's|FFEAD_PATH|'${IROOT}/ffead-cpp-6.0'|g' ${IROOT}/nginx-ffead-mongo/conf/nginx.conf
cp ${IROOT}/ffead-cpp-src/modules/nginx_mod_ffeadcpp/nginx.conf ${IROOT}/nginx-ffead-sql/conf/
sed -i 's|FFEAD_PATH|'${IROOT}/ffead-cpp-6.0-sql'|g' ${IROOT}/nginx-ffead-sql/conf/nginx.conf
