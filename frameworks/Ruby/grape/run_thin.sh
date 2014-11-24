#!/bin/bash

# We assume single-user installation as 
# done in our rvm.sh script and 
# in Travis-CI
source $HOME/.rvm/scripts/rvm

sed -i 's|  host:.*|  host: '"${DBHOST}"'|g' config/database.yml

rvm ruby-2.0.0-p0 do bundle exec thin start -C config/thin.yml &