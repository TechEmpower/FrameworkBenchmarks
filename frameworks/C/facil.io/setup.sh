#!/bin/bash

# enter root folder
cd $TROOT

if ! [ -d facil_app ] ; then
	bash <(curl -s https://raw.githubusercontent.com/boazsegev/facil.io/master/scripts/new/app) facil_app
fi

# recompile test
rm -r facil_app/src
mkdir facil_app/src
cp bench_app.c facil_app/src
cd facil_app
make -j build

# run test
cd tmp
./demo -b TFB-server -p 8080 -db "TFB-database" -w -1 -t 1 &
# step out
cd ../..
