#!/bin/bash


rm beyondj-launcher/src/main/resources/launchers/webapp-launchers/beyondj-service.war
mvn clean install
cp beyondj-service/target/beyondj-service.war beyondj-launcher/src/main/resources/launchers/webapp-launchers/
mvn clean install
rm -rf beyondj-launcher/deploy/
mkdir beyondj-launcher/deploy/

cp beyondj-launcher/target/beyondj.jar beyondj-launcher/deploy/
cd beyondj-launcher/deploy/
jar xvf beyondj.jar
rm beyondj.jar
jar -cvf0M beyondj.jar META-INF *
sudo cp beyondj.jar /var/www/html/beyondj.com/
cd ../../

