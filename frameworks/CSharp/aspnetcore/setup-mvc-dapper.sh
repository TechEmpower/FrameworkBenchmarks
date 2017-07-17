#!/bin/bash

fw_depends postgresql

source run-linux.sh 'MvcDbSingleQueryDapper,MvcDbMultiQueryDapper,MvcDbMultiUpdateDapper,MvcDbFortunesDapper' $(($(nproc)/2))
