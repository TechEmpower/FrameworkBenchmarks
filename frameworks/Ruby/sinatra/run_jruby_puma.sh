#!/bin/bash

THREAD_FACTOR=1

. $(dirname $0)/config/common_run.sh

fw_depends $DBTYPE rvm jruby-9.1

rvm use jruby-$JRUBY_VERSION

. $(dirname $0)/config/bundle_install.sh

bundle exec puma -t $MAX_CONCURRENCY:$MAX_CONCURRENCY -b tcp://0.0.0.0:8080 -e production &
