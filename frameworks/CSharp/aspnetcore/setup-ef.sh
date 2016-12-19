#!/bin/bash

fw_depends postgresql

source run-linux.sh ef $(($(nproc)/2))
