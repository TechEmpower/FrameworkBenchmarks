#!/bin/bash

fw_depends cutelyst

# configure
# DRIVER
# UWSGI
# NGINX
# PROCESS_OR_THREAD

echo DRIVER=${DRIVER}
echo UWSGI=${UWSGI}
echo NGINX=${NGINX}
echo QT_VERSION_MM=${QT_VERSION_MM}

if [ "${DRIVER}" == "QMYSQL" ]; then
  fw_depends mysql
elif [ "${DRIVER}" == "QPSQL" ]; then
  fw_depends postgresql
fi


mkdir ${IROOT}/cutelyst-benchmarks || true
cd ${IROOT}/cutelyst-benchmarks

# build
export CMAKE_PREFIX_PATH=/opt/qt${QT_VERSION_MM}:${IROOT}
cmake $TROOT -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=$IROOT
make -j $MAX_THREADS

if [ -n "${UWSGI}" ]; then
  cp -v ${TROOT}/config/config_socket.ini ${IROOT}/config.ini
  SEND_DATE=true
else
  cp -v ${TROOT}/config/config.ini ${IROOT}/config.ini
  SEND_DATE=false
fi

sed -i "s|Driver=.*|Driver=${DRIVER}|g" ${IROOT}/config.ini
sed -i "s|DatabaseHostName=.*|DatabaseHostName=${DBHOST}|g" ${IROOT}/config.ini
sed -i "s|SendDate=.*|SendDate=${SEND_DATE}|g" ${IROOT}/config.ini

export LD_LIBRARY_PATH=/opt/qt${QT_VERSION_MM}/lib:${IROOT}/lib/x86_64-linux-gnu/

if [ -n "${UWSGI}" ]; then
  uwsgi --ini ${IROOT}/config.ini --cutelyst-app ${IROOT}/cutelyst-benchmarks/src/libcutelyst_benchmarks.so ${PROCESS_OR_THREAD} $MAX_THREADS &
else
  ${IROOT}/bin/cutelyst-wsgi --ini ${IROOT}/config.ini -a ${IROOT}/cutelyst-benchmarks/src/libcutelyst_benchmarks.so ${PROCESS_OR_THREAD} $MAX_THREADS &
fi

# configure Nginx
if [ -n "${NGINX}" ]; then
  fw_depends nginx
  cp -v ${TROOT}/nginx.conf ${IROOT}/nginx.conf
  sed -i "s|include .*/conf/uwsgi_params;|include ${NGINX_HOME}/conf/uwsgi_params;|g" ${IROOT}/nginx.conf
  nginx -c ${IROOT}/nginx.conf
fi

