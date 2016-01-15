#!/bin/bash

sed -i 's|jdbc:mysql://.*:3306|jdbc:mysql://'"${DBHOST}"':3306|g' hello/grails-app/conf/DataSource.groovy

fw_depends java grails resin 

cd hello
grails -Dgrails.work.dir=${IROOT}/.grails -non-interactive -plain-output refresh-dependencies
grails -Dgrails.work.dir=${IROOT}/.grails -non-interactive -plain-output compile
grails -Dgrails.work.dir=${IROOT}/.grails prod -non-interactive -plain-output war
rm -rf $RESIN_HOME/webapps/*
cp target/hello-0.1.war $RESIN_HOME/webapps/grails.war
resinctl start
