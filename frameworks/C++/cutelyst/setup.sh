#!/bin/bash

sed -i 's|DatabaseHostName=.*|DatabaseHostName='"$DBHOST"'|g' config/config.ini

fw_depends cutelyst

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
uwsgi --ini ${TROOT}/config/config.ini --cutelyst-app ${IROOT}/cutelyst-benchmarks/src/libcutelyst_benchmarks.so -p $MAX_THREADS &
