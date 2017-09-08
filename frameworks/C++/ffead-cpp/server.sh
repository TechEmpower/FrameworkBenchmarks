#!/bin/sh

export MALLOC_CHECK_=0
IS_OS_DARWIN=`uname|tr '[A-Z]' '[a-z]'|awk 'index($0,"darwin") != 0 {print "darwin"}'`
if [ "$IS_OS_DARWIN" != "" ]; then
	export FFEAD_CPP_PATH=`cd "$(dirname server.sh)" && ABSPATH=$(pwd) && cd -`
else
	export FFEAD_CPP_PATH=`echo $(dirname $(readlink -f $0))`
fi

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
#chmod 700 $FFEAD_CPP_PATH/rtdcf/autotools/*
#/usr/sbin/setenforce 0
./CHS $FFEAD_CPP_PATH > ffead.log 2>&1