#!/bin/sh

rm -rf tmp

mkdir tmp
cd tmp

##########################################################
## install oatpp

MODULE_NAME="oatpp"

git clone --depth=1 https://github.com/oatpp/$MODULE_NAME

cd $MODULE_NAME
mkdir build
cd build

cmake ..
make install

cd ../../

##########################################################

cd ../

rm -rf tmp