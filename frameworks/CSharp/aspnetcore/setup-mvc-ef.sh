#!/bin/bash

source run-linux.sh 'MvcDbSingleQueryEf,MvcDbMultiQueryEf,MvcDbMultiUpdateEf,MvcDbFortunesEf' $(($(nproc)/2))
