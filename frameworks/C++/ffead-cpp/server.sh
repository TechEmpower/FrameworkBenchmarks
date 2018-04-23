#!/bin/sh

cd $FFEAD_CPP_PATH

export MALLOC_CHECK_=0

echo $FFEAD_CPP_PATH
export LD_LIBRARY_PATH=$FFEAD_CPP_PATH/lib:$LD_LIBRARY_PATH
echo $LD_LIBRARY_PATH
export PATH=$FFEAD_CPP_PATH/lib:$PATH
echo $PATH
export ODBCINI=$IROOT/odbc.ini
export ODBCSYSINI=$IROOT
echo $ODBCINI
echo $ODBCSYSINI
rm -f $FFEAD_CPP_PATH/rtdcf/*.d $FFEAD_CPP_PATH/rtdcf/*.o 
rm -f $FFEAD_CPP_PATH/*.cntrl
rm -f $FFEAD_CPP_PATH/tmp/*.sess
if [ ! -d tmp ]; then
mkdir tmp
fi
chmod 700 $FFEAD_CPP_PATH/CHS*
chmod 700 $FFEAD_CPP_PATH/resources/*.sh
chmod 700 $FFEAD_CPP_PATH/tests/*
chmod 700 $FFEAD_CPP_PATH/rtdcf/*
#/usr/sbin/setenforce 0

./CHS $FFEAD_CPP_PATH

wait
