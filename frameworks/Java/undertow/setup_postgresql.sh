#!/bin/bash

fw_depends postgresql

export UNDERTOW_ARGS="POSTGRESQL"

source ./setup.sh
