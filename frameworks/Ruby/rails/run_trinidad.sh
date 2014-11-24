#!/bin/bash

# We assume single-user installation as 
# done in our rvm.sh script and 
# in Travis-CI
source $HOME/.rvm/scripts/rvm

DB_HOST=${DBHOST} rvm jruby-1.7.13 do bundle exec trinidad --config config/trinidad.yml &