#!/bin/bash

. $(dirname $0)/config/common_run.sh

fw_depends $DBTYPE rvm ruby-2.4

rvm use ruby-$MRI_VERSION

. $(dirname $0)/config/bundle_install.sh

bundle exec unicorn -c config/mri_unicorn.rb -o 0.0.0.0 -p 8080 -E production &
