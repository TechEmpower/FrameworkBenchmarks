#!/bin/bash

fw_installed ffead-cpp-mongocdriver && return 0

fw_get -o mongo-c-driver-1.4.0.tar.gz https://github.com/mongodb/mongo-c-driver/releases/download/1.4.0/mongo-c-driver-1.4.0.tar.gz
rm -rf mongo-c-driver-1.4.0/
fw_untar mongo-c-driver-1.4.0.tar.gz
cd mongo-c-driver-1.4.0/
./configure --prefix=${IROOT} --libdir=${IROOT} --disable-automatic-init-and-cleanup
make && make install

touch ${IROOT}/ffead-cpp-mongocdriver.installed