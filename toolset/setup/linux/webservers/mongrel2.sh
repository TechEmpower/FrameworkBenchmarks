#!/bin/bash

fw_depends zeromq

fw_installed mongrel2 && return 0

MONGREL2=$IROOT/mongrel2
MONGREL2_VERSION="v1.8.1"

# TODO: Get away from apt-get
# Dependencies
sudo apt-get install -y sqlite3 libsqlite3-dev uuid uuid-runtime uuid-dev

# Update linker cache
sudo ldconfig -v

fw_get -o mongrel2.tar.gz https://github.com/mongrel2/mongrel2/tarball/$MONGREL2_VERSION
fw_untar mongrel2.tar.gz

# mongrel2 untars into this folder
mv mongrel2-mongrel2-aa2ecf8 mongrel2-install

# for zmq4, we update the following file manually (not in v1.8.1)
fw_get -O https://raw.githubusercontent.com/mongrel2/mongrel2/2cbf31de625b3476dfa541d6409f24364026f63a/src/zmq_compat.h
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

echo "export MONGREL2_HOME=${MONGREL2}" > $IROOT/mongrel2.installed
echo -e "export PATH=\$MONGREL2/bin:\$PATH" >> $IROOT/mongrel2.installed

source $IROOT/mongrel2.installed
