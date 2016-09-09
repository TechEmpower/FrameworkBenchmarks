#!/bin/bash

fw_depends cutelyst

sed -i 's|DatabaseHostName=.*|DatabaseHostName='"$DBHOST"'|g' config/config.ini
sed -i 's|SendDate=.*|SendDate=false|g' config/config.ini

cd $IROOT
mkdir cutelyst-benchmarks || true
cd cutelyst-benchmarks
rm -rf *

QT_VERSION_MM=56
export CMAKE_PREFIX_PATH=/opt/qt${QT_VERSION_MM}:${IROOT}

cmake $TROOT -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=$IROOT

make clean
make -j $MAX_THREADS

export LD_LIBRARY_PATH=/opt/qt${QT_VERSION_MM}/lib:${IROOT}/lib/x86_64-linux-gnu/
export CUTELYST_CONFIG=${TROOT}/config/config.ini

${IROOT}/bin/cutelyst-wsgi --http-socket :8080 -a ${IROOT}/cutelyst-benchmarks/src/libcutelyst_benchmarks.so -t $MAX_THREADS &
