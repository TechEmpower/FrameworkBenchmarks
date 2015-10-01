#!/bin/bash

fw_depends java8 maven

mvn package -U

java -jar beyondj-launcher/target/beyondj.jar system.platform.dbserver=${DBHOST} numInstances=10
