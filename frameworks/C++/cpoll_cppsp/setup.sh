#!/bin/bash

fw_depends postgresql-server-dev-9.3 cppsp

make clean
make
cd $CPPSP_HOME
./run_application $TROOT/www -g g++-4.8 -m /forcedynamic.cppsm &
