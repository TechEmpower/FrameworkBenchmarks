#!/bin/bash

# We assume single-user installation as 
# done in our rvm.sh script and 
# in Travis-CI
source $HOME/.rvm/scripts/rvm

rvm jruby-1.7.8 do bundle --jobs 4

DB_HOST=${DBHOST} rvm jruby-1.7.8 do bundle exec trinidad --config config/trinidad.yml &