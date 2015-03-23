#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/go1.4.2.installed)
[ ! "$RETCODE" == 0 ] || { return 0; }

fw_get https://storage.googleapis.com/golang/go1.4.2.linux-amd64.tar.gz
fw_untar go1.4.2.linux-amd64.tar.gz

touch ${IROOT}/go1.4.2.installed
