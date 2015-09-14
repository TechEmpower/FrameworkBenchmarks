#!/bin/bash

fw_depends java8 maven

mvn clean package
cd target
java -jar beyondj.jar &
