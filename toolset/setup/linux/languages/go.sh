#!/bin/bash
. ../toolset/setup/linux/bash_functions.sh

fw_exists go
[ $? -ne 0 ] || { echo "Go is installed!"; return 0; }

fw_get https://storage.googleapis.com/golang/go1.3.linux-amd64.tar.gz
fw_untar go1.3.linux-amd64.tar.gz
