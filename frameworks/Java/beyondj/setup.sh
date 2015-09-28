#!/bin/bash

fw_depends java8 maven

# rm beyondj-launcher/src/main/resources/launchers/webapp-launchers/beyondj-service.war
sudo chmod -R 777 .
mvn package
# cp beyondj-service/target/beyondj-service.war beyondj-launcher/src/main/resources/launchers/webapp-launchers/
# mvn clean package

java -jar beyondj-launcher/target/beyondj.jar system.platform.dbserver=${DBHOST} numInstances=10
