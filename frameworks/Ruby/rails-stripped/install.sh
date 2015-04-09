#!/bin/bash

fw_depends rvm nginx java7

if [ "$TRAVIS" = "true" ]
then
	rvmsudo rvm install ruby-2.0.0-p0
	rvmsudo rvm install jruby-1.7.8
else
	rvm install ruby-2.0.0-p0
	rvm install jruby-1.7.8
fi

rvm ruby-2.0.0-p0 do bundle install --gemfile=$TROOT/Gemfile-ruby --path vendor/bundle
rvm jruby-1.7.8 do bundle install --gemfile=$TROOT/Gemfile-jruby