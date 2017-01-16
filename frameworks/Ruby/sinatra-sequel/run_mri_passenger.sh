#!/bin/bash

. $(dirname $0)/config/common_run.sh

fw_depends $DBTYPE rvm ruby-2.4

rvm use ruby-$MRI_VERSION

. $(dirname $0)/config/bundle_install.sh

# TODO: https://github.com/phusion/passenger/issues/1916
export _PASSENGER_FORCE_HTTP_SESSION=true

# FWBM only... Passenger will auto-tune itself in production!
instances=$(ruby -r$(dirname $0)/config/auto_tune -e 'puts auto_tune.first')

bundle exec passenger start --log-level 1 \
  --engine builtin --disable-turbocaching --disable-security-update-check \
  --spawn-method direct --max-pool-size $instances --min-instances $instances --max-request-queue-size 1024 \
  --address 0.0.0.0 --port 8080 --environment production &
