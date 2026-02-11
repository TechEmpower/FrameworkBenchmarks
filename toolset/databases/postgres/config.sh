#!/bin/bash

set -e

cat /docker-entrypoint-initdb.d/postgresql.conf >> "${PGDATA}/postgresql.conf"

IO_WORKERS="$(( (`nproc` + 3) / 4 ))"

# The maximum valid value for the io_workers configuration parameter is 32.
if [[ "$IO_WORKERS" -lt 32 ]]; then
    sed -i "s/io_workers = 32/io_workers = ${IO_WORKERS}/" "${PGDATA}/postgresql.conf"
fi
