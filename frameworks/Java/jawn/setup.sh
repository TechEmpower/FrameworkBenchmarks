#!/bin/bash

fw_depends java8

sed -i 's|127.0.0.1|'${DBHOST}'|g' src/main/java/app/config/Database.java

./gradlew clean --daemon

./gradlew run -Pargs=production
