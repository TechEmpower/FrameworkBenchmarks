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

DB_HOST=${DBHOST} rvm ruby-2.1.2 do bundle exec puma -t 8:32 -w 8 --preload -b tcp://0.0.0.0:8080 -e production &