#!/bin/bash

# We assume single-user installation as 
# done in our rvm.sh script and 
# in Travis-CI
source $HOME/.rvm/scripts/rvm

sed -i 's|  host:.*|  host: '"${DBHOST}"'|g' config/database.yml

rvm ruby-2.0.0-p0 do bundle exec puma -t 8:32 -w 8 --preload -b tcp://0.0.0.0:8080 -e production &