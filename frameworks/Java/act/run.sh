#!/bin/bash

if [ ! -f target/dist/start ]; then
    mvn clean package
    cd target/dist
    unzip *.zip
else
    cd target/dist
fi
./start
