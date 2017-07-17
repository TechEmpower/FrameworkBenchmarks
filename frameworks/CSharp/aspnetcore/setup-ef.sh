#!/bin/bash

fw_depends postgresql

source run-linux.sh 'DbSingleQueryEf,DbMultiQueryEf,DbMultiUpdateEf,DbFortunesEf' $(($(nproc)/2))
