#!/bin/bash

fw_depends mysql java grails resin 

cd hello
grails -Dgrails.work.dir=${IROOT}/.grails -non-interactive -plain-output refresh-dependencies
grails -Dgrails.work.dir=${IROOT}/.grails -non-interactive -plain-output compile
grails -Dgrails.work.dir=${IROOT}/.grails prod -non-interactive -plain-output war
rm -rf $RESIN_HOME/webapps/*
cp target/hello-0.1.war $RESIN_HOME/webapps/grails.war
resinctl start
