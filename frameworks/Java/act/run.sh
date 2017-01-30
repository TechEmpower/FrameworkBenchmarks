#!/bin/bash

mvn -U clean package
cd target/dist
unzip *.zip
./start