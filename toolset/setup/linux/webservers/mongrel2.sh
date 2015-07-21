#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/mongrel2.installed)
[ ! "$RETCODE" == 0 ] || { return 0; }

fw_depends zeromq

# Dependencies
sudo apt-get install -y sqlite3 libsqlite3-dev uuid uuid-runtime uuid-dev

# Update linker cache
sudo ldconfig -v

fw_get https://github.com/zedshaw/mongrel2/tarball/v1.8.1 -o mongrel2.tar.gz
fw_untar mongrel2.tar.gz

# mongrel2 untars into this folder 
mv zedshaw-mongrel2-aa2ecf8 mongrel2-install

# for zmq4, we update the following file manually (not in v1.8.1)
fw_get -O https://raw.github.com/zedshaw/mongrel2/9b565eeea003783c47502c2d350b99c9684ce97c/src/zmq_compat.h
mv -f zmq_compat.h mongrel2-install/src/

cd mongrel2-install

# Do this in a subshell to avoid leaking env variables
(
  export PREFIX=${IROOT}/mongrel2
  export OPTFLAGS="-I$IROOT/zeromq-4.0.3/include"
  export OPTLIBS="-Wl,-rpath,$IROOT/zeromq-4.0.3/lib -L$IROOT/zeromq-4.0.3/lib"
  make clean all
  make install
)

touch ${IROOT}/mongrel2.installed
