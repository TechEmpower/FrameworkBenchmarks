#!/bin/bash

fw_depends postgresql

source run-linux.sh 'DbSingleQueryRaw,DbMultiQueryRaw,DbMultiUpdateRaw,DbFortunesRaw' $(($(nproc)/2))
