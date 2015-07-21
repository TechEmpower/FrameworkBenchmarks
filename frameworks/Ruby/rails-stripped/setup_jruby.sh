#!/bin/bash
source $IROOT/java7.installed
sed -i 's|host: .*|host: '"${DBHOST}"'|g' config/database.yml

# We assume single-user installation as 
# done in our rvm.sh script and 
# in Travis-CI
if [ "$TRAVIS" = "true" ]
then
	source /home/travis/.rvm/scripts/rvm
else
	source $HOME/.rvm/scripts/rvm
fi

rm -f Gemfile
cp Gemfile-jruby Gemfile
cp Gemfile-jruby.lock Gemfile.lock

rvm jruby-1.7.8 do bundle exec torqbox -b 0.0.0.0 -E production &