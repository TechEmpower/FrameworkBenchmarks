#!/bin/bash

. $(dirname $0)/config/common_run.sh

fw_depends $DBTYPE rvm ruby-2.4

rvm use ruby-$MRI_VERSION

. $(dirname $0)/config/bundle_install.sh

bundle exec puma -C config/mri_puma.rb -b tcp://0.0.0.0:8080 -e production &
