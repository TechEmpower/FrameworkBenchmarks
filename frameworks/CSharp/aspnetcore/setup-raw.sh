#!/bin/bash

source run-linux.sh 'DbSingleQueryRaw,DbMultiQueryRaw,DbMultiUpdateRaw,DbFortunesRaw' $(($(nproc)/2))
