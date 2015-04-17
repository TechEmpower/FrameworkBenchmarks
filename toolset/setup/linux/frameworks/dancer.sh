#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/dancer.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $IROOT/dancer.installed
  return 0; }

cpanm --notest --no-man-page Dancer Dancer::Plugin::Database DBI DBD::mysql JSON::XS Plack Starman

echo "" > $DANCER.installed

source $IROOT/dancer.installed
