#!/bin/bash

fw_depends postgresql

source run-linux.sh 'MvcDbSingleQueryEf,MvcDbMultiQueryEf,MvcDbMultiUpdateEf,MvcDbFortunesEf' $(($(nproc)/2))
