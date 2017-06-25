#!/bin/bash

fw_depends postgresql

source run-linux.sh 'DbSingleQueryDapper,DbMultiQueryDapper,DbMultiUpdateDapper,DbFortunesDapper' $(($(nproc)/2))
