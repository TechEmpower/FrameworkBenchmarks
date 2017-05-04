#!/bin/bash

fw_depends postgresql

source run-linux.sh 'MvcDbSingleQueryRaw,MvcDbMultiQueryRaw,MvcDbMultiUpdateRaw,MvcDbFortunesRaw' $(($(nproc)/2))
