#!/usr/bin/env bash
set -euox pipefail

chown -R $USER_ID /var/run/

gosu $USER_ID python2 /FrameworkBenchmarks/toolset/run-tests.py "$@"