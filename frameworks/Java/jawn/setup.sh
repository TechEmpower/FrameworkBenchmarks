#!/bin/bash

fw_depends java8

./gradlew clean --daemon

./gradlew run -Pargs=production,$DBHOST
