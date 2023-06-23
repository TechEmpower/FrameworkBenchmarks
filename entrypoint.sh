#!/bin/bash

set -eoux pipefail

chown -R "$USER_ID" /var/run
# Drop permissions of user to match those of the host system
gosu "$USER_ID" python3 /FrameworkBenchmarks/toolset/run-tests.py "$@"
