#!/bin/bash

fw_depends postgres

source run-linux.sh dapper $(($(nproc)/2))
