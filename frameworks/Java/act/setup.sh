#!/bin/bash

fw_depends mysql postgresql mongodb java maven

mvn clean package
cd target/dist
unzip *.zip
./start -Dmongo.host=${DBHOST} -Dmysql.host=${DBHOST} -Dpgsql.host=${DBHOST}