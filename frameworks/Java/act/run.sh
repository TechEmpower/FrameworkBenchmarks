#!/bin/bash

mvn clean package
cd target/dist
unzip *.zip
./start
