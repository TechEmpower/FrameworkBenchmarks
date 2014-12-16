#!/bin/bash

mvn clean package

java -jar target/grizzly-jersey-example.jar -dbhost ${DBHOST} &