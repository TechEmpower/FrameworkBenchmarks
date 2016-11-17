#!/bin/bash

if [[ $LOGDIR == *postgres* ]] ; then
  DBTYPE=postgresql
else
  DBTYPE=mysql
fi

fw_depends $DBTYPE rvm ruby-2.4

rvm ruby-$MRI_VERSION do \
  bundle install --jobs=4 --gemfile=$TROOT/Gemfile --path=vendor/bundle

export DBTYPE
export MAX_CONCURRENCY # used by config/mri_puma.rb

rvm ruby-$MRI_VERSION do \
  bundle exec puma -C config/mri_puma.rb -b tcp://0.0.0.0:8080 -e production &
