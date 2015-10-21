#!/bin/sh
export LD_LIBRARY_PATH=../lib:$LD_LIBRARY_PATH
export PATH=../lib:$PATH
#/usr/sbin/setenforce 0
./tests