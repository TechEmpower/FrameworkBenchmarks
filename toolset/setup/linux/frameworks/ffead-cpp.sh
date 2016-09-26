#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/ffead-cpp.installed)
[ ! "$RETCODE" == 0 ] || { return 0; }

sudo apt-get install -y uuid-dev unixodbc unixodbc-dev

fw_get -o ffead-cpp-2.0.tar.gz https://github.com/sumeetchhetri/ffead-cpp/releases/download/v2.0-Draft-TLV-Fixed-TE_Benchmark/ffead-cpp-2.0-tlfixed-bin.tar.gz
fw_untar ffead-cpp-2.0.tar.gz

rm -rf ${TROOT}/ffead-cpp-2.0-bin
cp -R ffead-cpp-2.0-bin/ ${TROOT}
mv ${TROOT}/ffead-cpp-2.0-bin ${TROOT}/ffead-cpp-2.0
rm -rf ffead-cpp-2.0/

sudo chown -R testrunner:testrunner ${TROOT}/ffead-cpp-2.0
sudo chmod -R g+rw ${TROOT}/ffead-cpp-2.0

wget https://github.com/mongodb/mongo-c-driver/releases/download/1.1.10/mongo-c-driver-1.1.10.tar.gz
tar -xzf mongo-c-driver-1.1.10.tar.gz
cd mongo-c-driver-1.1.10/
./configure --prefix=${IROOT} --libdir=${IROOT}
make && sudo make install

touch ${IROOT}/ffead-cpp.installed

