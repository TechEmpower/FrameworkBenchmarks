#!/bin/bash

fw_depends dart

pub upgrade

dart server.dart -a 0.0.0.0 -p 8080 -d ${CPU_COUNT} -i ${CPU_COUNT} &
