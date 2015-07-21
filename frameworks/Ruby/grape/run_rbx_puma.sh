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

sed -i 's|  host:.*|  host: '"${DBHOST}"'|g' config/database.yml

rvm rbx-2.2.10 do bundle exec puma -b tcp://0.0.0.0:8080 -e production &