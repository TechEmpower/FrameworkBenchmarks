#!/bin/bash

fw_depends ffead-cpp-apache-mysql

rm -f $FFEAD_CPP_PATH/*.cntrl
rm -f $FFEAD_CPP_PATH/tmp/*.sess
apachectl restart > ffead.log 2>&1
