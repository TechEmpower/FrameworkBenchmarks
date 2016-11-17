#!/bin/bash

if [[ $LOGDIR == *postgres* ]] ; then
  DBTYPE=postgresql
else
  DBTYPE=mysql
fi

fw_depends $DBTYPE rvm jruby-9.1

rvm jruby-$JRUBY_VERSION do \
  bundle install --jobs=4 --gemfile=$TROOT/Gemfile --path=vendor/bundle

export DBTYPE
export JRUBY_OPTS="-J-Xmn512m -J-Xms2048m -J-Xmx2048m -J-server -J-XX:+UseSerialGC -J-Djava.net.preferIPv4Stack=true"

rvm jruby-$JRUBY_VERSION do \
  bundle exec puma -t $MAX_CONCURRENCY:$MAX_CONCURRENCY -b tcp://0.0.0.0:8080 -e production &
