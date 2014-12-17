#!/bin/bash

sed -i 's|  host:.*|  host:'"${DBHOST}"'|g' config/database.yml

# We assume single-user installation as 
# done in our rvm.sh script and 
# in Travis-CI
source $HOME/.rvm/scripts/rvm

rvm rbx-2.2.10 do bundle exec puma -b tcp://0.0.0.0:8080 -e production &