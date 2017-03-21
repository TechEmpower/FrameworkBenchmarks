#!/bin/bash

fw_depends postgresql

source run-linux.sh dapper $(($(nproc)/2))
