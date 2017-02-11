#!/bin/bash

sed -i 's|DATABASE_HOST|'"${DBHOST}"'|g' src/main/resources/config/mysql.json
sed -i 's|DATABASE_HOST|'"${DBHOST}"'|g' src/main/resources/config/postgres.json

fw_depends postgresql mysql java maven

mvn clean package
cd target
java -jar techempower-1.0.0.jar &
