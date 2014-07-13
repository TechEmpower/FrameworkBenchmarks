#!/bin/bash

# TODO Install RVM inside installs directory. Perhaps use rvm-path

curl -L get.rvm.io | bash -s head --auto-dotfiles
echo rvm_auto_reload_flag=2 >> ~/.rvmrc

RETCODE=$(fw_exists ~/.rvm/rubies/ruby-2.0.0-p0/)
[ ! "$RETCODE" == 0 ] || { return 0; }

. ~/.rvm/scripts/rvm
. ~/.profile

# Disable trap and errtrace, as a 'normal' ruby 
# install internally has errors, which cause our 
# installer system to report a potential ruby install 
# failure
set +E
trap '' ERR

rvm install 2.0.0-p0
rvm 2.0.0-p0 do gem install bundler

# Install JRuby 
rvm install jruby-1.7.8
rvm jruby-1.7.8 do gem install bundler

# Turn errtrace back on (the ERR trap will 
# be restored by fw_depends)
set -E
