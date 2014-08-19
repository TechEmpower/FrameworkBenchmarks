#!/bin/bash

fw_depends rvm nginx java

rvm install ruby-2.0.0-p0
rvm ruby-2.0.0-p0 do bundle install --gemfile=$TROOT/Gemfile-ruby

rvm install jruby-1.7.8
rvm jruby-1.7.8 do bundle install --gemfile=$TROOT/Gemfile-jruby
