#!/bin/bash

# Ensure we don't accidentally (try to) use gems for the wrong platform.
rm -f $TROOT/Gemfile.lock

bundle install --jobs=4 --gemfile=$TROOT/Gemfile --path=$IROOT/sinatra/bundle
