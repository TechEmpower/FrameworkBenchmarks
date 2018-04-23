#!/bin/bash

source run-linux.sh 'DbSingleQueryDapper,DbMultiQueryDapper,DbMultiUpdateDapper,DbFortunesDapper' $(($(nproc)/2))
