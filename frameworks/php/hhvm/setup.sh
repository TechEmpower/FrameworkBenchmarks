#!/bin/bash

# Setup script
cat ../config/create.sql | mysql -u root

# for testing create a symlink
WDIR=$(cd .. && pwd)
cd /tmp/ && ln -s $WDIR FrameworkBenchmarks && cd -

# next invoke ./run.sh
