#!/bin/bash

fw_depends postgresql mysql java maven

sed -i 's|mysql://.*:3306|mysql://'"${DBHOST}"':3306|g' conf/application.conf
sed -i 's|postgresql://.*:5432|postgresql://'"${DBHOST}"':5432|g' conf/application.conf

mvn -U clean package
cd target
java -Dlogback.configurationFile="conf/logback.xml" -server  -Xms1g -Xmx2g -XX:+UseG1GC -XX:+AggressiveOpts -XX:-UseBiasedLocking -XX:+UseStringDeduplication -classpath "./proteus-techempower-1.0.0.jar:lib/*" io.sinistral.ExampleApplication &