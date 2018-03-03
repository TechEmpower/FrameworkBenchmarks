#!/bin/bash

fw_depends mongodb

export UNDERTOW_ARGS="MONGODB"

source ./setup.sh
