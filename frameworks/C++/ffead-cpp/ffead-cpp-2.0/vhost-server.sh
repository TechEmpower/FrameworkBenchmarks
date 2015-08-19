#!/bin/sh

#/usr/sbin/setenforce 0
./CHS "$@" > ffead."$5".log 2>&1 &