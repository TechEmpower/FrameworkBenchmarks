#!/bin/bash

fw_depends postgresql java maven

mvn clean package

cd target
java -server -Xms512m -Xmx2g -jar jooby-1.0.jar &
