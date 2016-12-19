#!/bin/bash

fw_depends postgres

source run-linux.sh raw $(($(nproc)/2))
