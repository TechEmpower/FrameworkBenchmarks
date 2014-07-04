#!/bin/bash

fw_exists ~/.rvm/rubies/jruby-1.7.8/
[ $? -ne 0 ] || { return 0; }

# Instead of installing separately, we install JRuby 
# whenever we install Ruby
fw_depends ruby
