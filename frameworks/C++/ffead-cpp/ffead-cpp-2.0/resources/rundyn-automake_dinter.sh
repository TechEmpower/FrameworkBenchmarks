#!/bin/sh

FFEAD_CPPPTH=$1
export FFEAD_CPP_PATH=${FFEAD_CPPPTH}

cd $FFEAD_CPP_PATH/rtdcf/autotools
make libdinter.la
cp -f .libs/*dinter* $FFEAD_CPP_PATH/lib/