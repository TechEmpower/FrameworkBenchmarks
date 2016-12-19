#!/bin/bash

fw_depends postgresql

source run-linux.sh raw $(($(nproc)/2))
