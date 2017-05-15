#!/bin/bash

THREAD_FACTOR=2

. $(dirname $0)/config/common_run.sh

fw_depends $DBTYPE rvm jruby-9.1

rvm use jruby-$JRUBY_VERSION

. $(dirname $0)/config/bundle_install.sh

bundle exec torquebox run --io-threads $(( MAX_CONCURRENCY / 2 )) --worker-threads $MAX_CONCURRENCY -b 0.0.0.0 -p 8080 -e production &
