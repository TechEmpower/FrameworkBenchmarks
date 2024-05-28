#!/bin/bash

set -e

cat /tmp/postgresql.conf >> "${PGDATA}/postgresql.conf"
