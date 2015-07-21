#!/bin/bash

fw_depends rvm nginx java7

if [ "$TRAVIS" = "true" ]
then
	rvmsudo rvm install 2.1.2
	rvmsudo rvm install jruby-1.7.13
	rvmsudo rvm install rbx-2.2.10
else
	rvm install 2.1.2
	rvm install jruby-1.7.13
	rvm install rbx-2.2.10
fi

rvm 2.1.2 do bundle install --gemfile=$TROOT/Gemfile
rvm jruby-1.7.13 do bundle install --gemfile=$TROOT/Gemfile

export LC_ALL=en_US.UTF-8 
export LANG=en_US.UTF-8
rvm rbx-2.2.10 do bundle install --gemfile=$TROOT/Gemfile
