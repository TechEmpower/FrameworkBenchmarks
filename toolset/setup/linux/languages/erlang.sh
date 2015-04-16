#!/bin/bash

OTP_SRC="otp_src_17.5"
RETCODE=$(fw_exists ${IROOT}/erlang.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $IROOT/erlang.installed
  return 0; }

fw_get http://www.erlang.org/download/${OTP_SRC}.tar.gz
fw_untar ${OTP_SRC}.tar.gz

(
	cd $OTP_SRC
	export ERL_TOP=`pwd`
	./configure --prefix=$IROOT/erlang --without-termcap
	make
	make install
)

echo -e "export PATH=$IROOT/erlang/bin:\$PATH" >> $IROOT/erlang.installed

source $IROOT/erlang.installed
