#!/bin/bash

fw_depends ffead-cpp-emb-postgresql

chmod 755 $IROOT/ffead-cpp-2.0/*.sh
rm -f $FFEAD_CPP_PATH/*.cntrl
rm -f $FFEAD_CPP_PATH/tmp/*.sess
cd $IROOT/ffead-cpp-2.0
./server.sh
