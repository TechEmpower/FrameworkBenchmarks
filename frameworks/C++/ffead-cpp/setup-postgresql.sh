#!/bin/bash

fw_depends ffead-cpp-emb-postgresql

chmod 755 $FFEAD_CPP_PATH/*.sh
rm -f $FFEAD_CPP_PATH/*.cntrl
rm -f $FFEAD_CPP_PATH/tmp/*.sess
cd $FFEAD_CPP_PATH
./server.sh
