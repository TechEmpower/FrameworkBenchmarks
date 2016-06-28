#!/bin/bash

fw_depends rvm ruby-2.0

sed -i 's|  host:.*|  host: '"${DBHOST}"'|g' config/database.yml

rvm ruby-$MRI_VERSION do bundle install --jobs=4 --gemfile=$TROOT/Gemfile --path=vendor/bundle

rvm ruby-$MRI_VERSION do bundle exec thin start -C config/thin.yml &
