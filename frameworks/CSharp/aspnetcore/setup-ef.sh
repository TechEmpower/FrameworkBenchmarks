#!/bin/bash

source run-linux.sh 'DbSingleQueryEf,DbMultiQueryEf,DbMultiUpdateEf,DbFortunesEf' $(($(nproc)/2))
