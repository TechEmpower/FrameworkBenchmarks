#!/bin/bash

fw_depends postgresql java8

./gradlew clean

./gradlew --no-daemon --refresh-dependencies run -Pargs=8080,production
