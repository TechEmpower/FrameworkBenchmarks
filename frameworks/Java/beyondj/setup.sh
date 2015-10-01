#!/bin/bash

fw_depends java8 maven

sudo chmod -R 775 .

mvn package

java -jar beyondj-launcher/target/beyondj.jar system.platform.dbserver=${DBHOST} numInstances=10
