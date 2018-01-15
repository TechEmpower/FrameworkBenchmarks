#!/bin/bash

fw_depends postgresql mysql java maven

mvn clean package
cd target
java -agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=5005 -server -Xms512m -Xmx2g -jar techempower-1.0.0.jar &
