#!/bin/bash

fw_depends mysql java8 maven

mvn -Peclipselink_mysql clean package
cd target/dist
unzip *.zip


APP_ENTRY=com.techempower.act.AppEntry

DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
BASE=$DIR/target/dist
if [[ $DIR == *"dist" ]]; then
  BASE=$DIR
fi
CP=$BASE/classes:$BASE/lib/*
echo
echo CLASSPATH: $CP
echo

JAVA_OPTS="-Djava.security.egd=file:/dev/./urandom -Xms1G -Xmx1G -Xss320k -XX:+UseNUMA -XX:+UseParallelGC -XX:+AggressiveOpts"

java -server $JAVA_OPTS -Dapp.mode=prod -Dapp.nodeGroup=$GROUP -Dprofile=eclipselink_mysql -Dxio.worker_threads.max=256 -Dmysql.host=${DBHOST} -cp "$CP" $APP_ENTRY 
