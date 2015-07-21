#!/bin/bash

source $IROOT/java7.installed

# We assume single-user installation as 
# done in our rvm.sh script and 
# in Travis-CI
if [ "$TRAVIS" = "true" ]
then
	source /home/travis/.rvm/scripts/rvm
else
	source $HOME/.rvm/scripts/rvm
fi

rvm jruby-1.7.8 do bundle --jobs 4

DB_HOST=${DBHOST} rvm jruby-1.7.8 do bundle exec puma -C config/puma.rb &