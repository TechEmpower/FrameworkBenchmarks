#!/bin/bash

fw_depends java maven

mvn clean package
java -jar target/permeagility-0.8.0-benchmark.jar &
