#!/bin/bash

set -eo pipefail -u

chown -LR "$USER_ID" /var/run
# Drop permissions of user to match those of the host system
gosu "$USER_ID" python3 "${FWROOT}/toolset/run-tests.py" "$@"
