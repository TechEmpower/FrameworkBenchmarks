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

DB_HOST=${DBHOST} rvm ruby-2.0.0-p0 do bundle exec puma -C config/puma.rb -w 8 --preload &