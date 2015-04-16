#!/bin/bash

fw_depends java7 maven

mvn clean package

java -jar target/dropwizard-mongodb-0.0.1-SNAPSHOT.jar server hello-world.yml &
