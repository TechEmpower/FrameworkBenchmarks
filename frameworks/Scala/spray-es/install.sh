#!/bin/bash

fw_depends java8 sbt

export JAVA_HOME=/opt/java8
export PATH=$JAVA_HOME/bin:$PATH:$IROOT/sbt/bin

sbt_opts='-J-XX:+UseG1GC -J-Xmx2G -J-XX:MaxMetaspaceSize=512m'

cd $TROOT
sbt $sbt_opts package
sbt $sbt_opts assembly
