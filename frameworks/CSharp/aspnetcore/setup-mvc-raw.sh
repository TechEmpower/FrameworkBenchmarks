#!/bin/bash

source run-linux.sh 'MvcDbSingleQueryRaw,MvcDbMultiQueryRaw,MvcDbMultiUpdateRaw,MvcDbFortunesRaw' $(($(nproc)/2))
