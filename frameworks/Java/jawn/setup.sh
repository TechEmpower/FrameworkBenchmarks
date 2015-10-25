#!/bin/bash

fw_depends java8

./gradlew clean

sed -i 's|127.0.0.1|'${DBHOST}'|g' src/main/java/app/config/Database.java

./gradlew run --refresh-dependencies -Pargs=production
