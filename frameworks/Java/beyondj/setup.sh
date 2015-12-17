#!/bin/bash

fw_depends java8 maven

#mvn install

java -jar beyondj-launcher/target/beyondj.jar system.platform.dbserver=${DBHOST} numInstances=10
