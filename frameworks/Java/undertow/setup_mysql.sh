#!/bin/bash

fw_depends mysql

export UNDERTOW_ARGS="MYSQL"

source ./setup.sh
