#!/bin/bash

fw_depends java

./gradlew clean

sed -i 's|127.0.0.1|'${DBHOST}'|g' src/main/java/app/config/Database.java

./gradlew --refresh-dependencies run -Pargs=production
