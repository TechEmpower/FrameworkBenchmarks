#!/bin/bash

fw_depends crystal

crystal deps install

crystal build --release server-postgres.cr

KEMAL_ENV=production ./server-postgres &
