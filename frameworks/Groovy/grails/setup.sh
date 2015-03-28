#!/bin/bash
export JAVA_HOME=/usr/lib/jvm/java-1.7.0-openjdk-amd64
export RESIN_HOME=${IROOT}/resin-4.0.41

export GRAILS_PATH=${IROOT}/grails-2.4.4/bin/

sed -i 's|jdbc:mysql://.*:3306|jdbc:mysql://'"${DBHOST}"':3306|g' hello/grails-app/conf/DataSource.groovy

export PATH=${GRAILS_PATH}:$PATH
export GRAILS_AGENT_CACHE_DIR=${IROOT}/.grails/.slcache

cd hello
grails -Dgrails.work.dir=${IROOT}/.grails -non-interactive -plain-output refresh-dependencies
grails -Dgrails.work.dir=${IROOT}/.grails -non-interactive -plain-output compile
grails -Dgrails.work.dir=${IROOT}/.grails prod -non-interactive -plain-output war
rm -rf $RESIN_HOME/webapps/*
cp target/hello-0.1.war $RESIN_HOME/webapps/grails.war
$RESIN_HOME/bin/resinctl start
