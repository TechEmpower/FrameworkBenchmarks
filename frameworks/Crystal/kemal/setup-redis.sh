#!/bin/bash

fw_depends crystal

crystal deps install

crystal build --release server-redis.cr

KEMAL_ENV=production ./server-redis &
