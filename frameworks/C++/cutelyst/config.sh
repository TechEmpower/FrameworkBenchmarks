#!/bin/bash

fw_depends cutelyst

# configure
# DRIVER
# UWSGI
# NGINX
# PROCESS_OR_THREAD
# CUTELYST_EVENT_LOOP_EPOLL

echo DRIVER=${DRIVER}
echo UWSGI=${UWSGI}
echo NGINX=${NGINX}
echo QT_VERSION_MM=${QT_VERSION_MM}
echo CUTELYST_EVENT_LOOP_EPOLL=${CUTELYST_EVENT_LOOP_EPOLL}
echo BALANCER=${BALANCER}

if [ "${DRIVER}" == "QMYSQL" ]; then
  fw_depends mysql
elif [ "${DRIVER}" == "QPSQL" ]; then
  fw_depends postgresql
fi

CROOT=${IROOT}/cutelyst

mkdir -p ${CROOT}/benchmarks || true
cd ${CROOT}/benchmarks

# build
export CMAKE_PREFIX_PATH=/opt/qt${QT_VERSION_MM}:${IROOT}
cmake $TROOT -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=$IROOT
make -j $CPU_COUNT


if [ -n "${UWSGI}" ]; then
  cp -v ${TROOT}/config/config_socket.ini ${CROOT}/config.ini
  SEND_DATE=true
else
  cp -v ${TROOT}/config/config.ini ${CROOT}/config.ini
  SEND_DATE=false
fi

sed -i "s|Driver=.*|Driver=${DRIVER}|g" ${CROOT}/config.ini
sed -i "s|DatabaseHostName=.*|DatabaseHostName=${DBHOST}|g" ${CROOT}/config.ini
sed -i "s|SendDate=.*|SendDate=${SEND_DATE}|g" ${CROOT}/config.ini

export LD_LIBRARY_PATH=/opt/qt${QT_VERSION_MM}/lib:${CROOT}/lib/x86_64-linux-gnu/

if [ -n "${UWSGI}" ]; then
  uwsgi --ini ${IROOT}/config.ini --cutelyst-app ${IROOT}/cutelyst-benchmarks/src/libcutelyst_benchmarks.so ${PROCESS_OR_THREAD} $CPU_COUNT &
else
  ${IROOT}/bin/cutelyst-wsgi --ini ${IROOT}/config.ini -a ${IROOT}/cutelyst-benchmarks/src/libcutelyst_benchmarks.so ${PROCESS_OR_THREAD} $CPU_COUNT --socket-timeout 0 &
fi

# configure Nginx
if [ -n "${NGINX}" ]; then
  fw_depends nginx
  cp -v ${TROOT}/nginx.conf ${CROOT}/nginx.conf
  sed -i "s|include .*/conf/uwsgi_params;|include ${NGINX_HOME}/conf/uwsgi_params;|g" ${CROOT}/nginx.conf
  nginx -c ${CROOT}/nginx.conf
fi

