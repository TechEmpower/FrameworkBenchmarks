#!/bin/bash

fw_depends rvm nginx java7 maven

if [ "$TRAVIS" = "true" ]
then
	rvmsudo rvm install ruby-2.0.0-p0
	rvmsudo rvm install jruby-1.7.8
	rvmsudo rvm install rbx-2.2.10
else
	rvm install ruby-2.0.0-p0
	rvm install jruby-1.7.8
	rvm install rbx-2.2.10
fi

rvm ruby-2.0.0-p0 do bundle install --gemfile=$TROOT/Gemfile --path vendor/bundle
rvm jruby-1.7.8 do bundle install --gemfile=$TROOT/Gemfile

export LC_ALL=en_US.UTF-8 
export LANG=en_US.UTF-8
rvm rbx-2.2.10 do bundle install --gemfile=$TROOT/Gemfile
