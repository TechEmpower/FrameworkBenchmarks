#!/bin/bash

source run-linux.sh 'MvcDbSingleQueryDapper,MvcDbMultiQueryDapper,MvcDbMultiUpdateDapper,MvcDbFortunesDapper' $(($(nproc)/2))
