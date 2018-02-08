#!/bin/bash

# enter root folder
cd $TROOT

# remove existing installation, if any
if [ -d facil_app ] ; then
	rm -R facil_app
fi

# create new installation folder
mkdir facil_app
cd facil_app

# Download and unpack

curl -s -o facil.io.tar.gz -LJO https://api.github.com/repos/boazsegev/facil.io/tarball/0.6.0.beta.6
tar --strip-components=1 -xzf facil.io.tar.gz
if [ $? -ne 0 ]; then echo "Couldn't extract tar."; exit 1; fi
rm facil.io.tar.gz
./scripts/new/cleanup
cd ..


# compile test
rm -r facil_app/src
mkdir facil_app/src
cp bench_app.c facil_app/src
cd facil_app

# we don't need more than 32K concurrent connections
export CFLAGS="-DLIB_SOCK_MAX_CAPACITY=32768"

# Build the app
make -j build

# Run the upp
cd tmp
./demo -p 8080 -db "TFB-database" -w -1 -t 1 &
# step out of app folder
cd ../..
 
