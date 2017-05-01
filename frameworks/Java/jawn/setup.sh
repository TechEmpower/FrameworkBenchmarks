#!/bin/bash

fw_depends mysql java

./gradlew clean

./gradlew --refresh-dependencies run -Pargs=production
