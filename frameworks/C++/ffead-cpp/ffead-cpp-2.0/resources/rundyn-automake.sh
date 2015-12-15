#!/bin/sh

FFEAD_CPPPTH=$1
export FFEAD_CPP_PATH=${FFEAD_CPPPTH}

cd $FFEAD_CPP_PATH/rtdcf/autotools
make clean
make all
cp -f .libs/*inter* $FFEAD_CPP_PATH/lib/