#!/bin/bash

fw_depends mongodb

export UNDERTOW_ARGS="MONGODB_ASYNC"

source ./setup.sh
