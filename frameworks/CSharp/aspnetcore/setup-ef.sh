#!/bin/bash

fw_depends postgres

source run-linux.sh ef $(($(nproc)/2))
