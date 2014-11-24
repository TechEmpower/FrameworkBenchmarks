#!/bin/bash

# We assume single-user installation as 
# done in our rvm.sh script and 
# in Travis-CI
source $HOME/.rvm/scripts/rvm

sed -i 's|  host:.*|  host: '"${DBHOST}"'|g' config/database.yml

rvm jruby-1.7.8 do bundle exec torqbox -b 0.0.0.0 -E production &