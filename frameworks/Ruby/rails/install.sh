#!/bin/bash

fw_depends rvm nginx java

rvm install 2.1.2
rvm 2.1.2 do bundle install --gemfile=$TROOT/Gemfile

rvm install jruby-1.7.13
rvm jruby-1.7.13 do bundle install --gemfile=$TROOT/Gemfile

rvm install rbx-2.2.10
rvm rbx-2.2.10 do bundle install --gemfile=$TROOT/Gemfile