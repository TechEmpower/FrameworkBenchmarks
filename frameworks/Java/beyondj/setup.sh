#!/bin/bash

fw_depends java8 maven

mvn clean package

java -jar beyondj-launcher/target/beyondj.jar &
