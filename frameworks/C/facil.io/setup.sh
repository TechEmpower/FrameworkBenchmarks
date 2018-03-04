#!/bin/bash

fw_depends gcc-6

# enter root folder
cd $TROOT

# perform common installations (if required)
$TROOT/setup-common.sh

#### Compile test

# Remove boiler-plate and copy app to src folder
cp -f bench_app.c facil_app/src/app.c
cd facil_app

# set compiler
export CC="gcc-6"

# we don't need more than 24K concurrent connections
export CFLAGS="${CFLAGS} -DLIB_SOCK_MAX_CAPACITY=24576"

# Build the app
make clean
make -j build

# Run the upp
#
# Know options:
#   -? command line options help (lists options I didn't list here).
#   -w (worker processes). negtive values are replaced by automatic core detection (up to 7 cores).
#   -t (threads per worker). negtive values are replaced by automatic core detection (up to 7 cores).
#   -p port to listen to (should be 0 for Unix sockets).
#   -b address to bind to (accepts Unix socket addresses by using absolute paths (i.e. "/tmp/my_app_sock").
#   -dbp database port used for database connections (not implemented).
#   -db database address used for database connections (not implemented).
#
cd tmp
./demo -p 8080 -w -1 -t -1 -db "TFB-database" &
# step out of app folder
cd ../..
 
