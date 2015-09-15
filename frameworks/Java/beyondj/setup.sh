#!/bin/bash

fw_depends java8 maven

rm beyondj-launcher/src/main/resources/launchers/webapp-launchers/beyondj-service.war
rm beyondj-launcher/src/main/resources/launchers/jar-launchers/beyondj-data-loader.jar

mvn clean package

cp beyondj-service/target/beyondj-service.war beyondj-launcher/src/main/resources/launchers/webapp-launchers/
cp beyondj-data-loader/target/beyondj-data-loader.jar beyondj-launcher/src/main/resources/launchers/jar-launchers/

mvn clean package

java -jar beyondj-launcher/target/beyondj.jar &
