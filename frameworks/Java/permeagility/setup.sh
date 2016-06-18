#!/bin/bash

fw_depends java maven

mvn clean package
java -jar target/permeagility-0.1.0-SNAPSHOT-benchmark.jar &
