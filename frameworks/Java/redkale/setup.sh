#!/bin/bash

fw_depends java maven

mvn clean package

java -DAPP_HOME=./ -jar target/redkale-benchmark-0.0.1.jar &
