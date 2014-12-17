#!/bin/bash

# We assume single-user installation as 
# done in our rvm.sh script and 
# in Travis-CI
source $HOME/.rvm/scripts/rvm

DB_HOST=${DBHOST} rvm ruby-2.1.2 do bundle exec puma -t 8:32 -w 8 --preload -b tcp://0.0.0.0:8080 -e production &