#!/bin/bash

fw_depends postgresql dart

pub upgrade

dart server.dart -a 0.0.0.0 -p 8080 -d ${MAX_CONCURRENCY} -i ${CPU_COUNT} &
