#!/bin/bash

fw_exists go
[ $? -ne 0 ] || { return 0; }

fw_get https://storage.googleapis.com/golang/go1.3.linux-amd64.tar.gz
fw_untar go1.3.linux-amd64.tar.gz
