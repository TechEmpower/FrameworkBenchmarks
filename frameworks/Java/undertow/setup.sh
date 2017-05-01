#!/bin/bash

fw_depends java maven

mvn clean package

java -jar target/hello-undertow.jar $UNDERTOW_ARGS
