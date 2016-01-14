#!/bin/bash

fw_depends java sbt

sbt_opts='-J-XX:+UseG1GC -J-Xmx2G -J-XX:MaxMetaspaceSize=512m'

sbt $sbt_opts package -batch
sbt $sbt_opts assembly -batch

java -Dtfb.db_host=$DBHOST -jar target/scala-2.11/spray-es-assembly-0.1.jar &
