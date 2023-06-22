#!/usr/bin/env bash
set -euox pipefail

chown -R $USER_ID /var/run/

gosu $USER_ID python3 /FrameworkBenchmarks/toolset/run-tests.py "$@"
