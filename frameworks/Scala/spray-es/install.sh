#!/bin/bash

fw_depends java8 sbt

source $IROOT/java8.installed
export PATH=$IROOT/sbt/bin:$PATH

sbt_opts='-J-XX:+UseG1GC -J-Xmx2G -J-XX:MaxMetaspaceSize=512m'

cd $TROOT
sbt $sbt_opts package
sbt $sbt_opts assembly
