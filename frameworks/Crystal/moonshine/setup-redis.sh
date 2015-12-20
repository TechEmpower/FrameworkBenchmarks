#!/bin/bash

#fw_depends crystal

curl http://dist.crystal-lang.org/apt/setup.sh | sudo bash

sudo apt-get install crystal

crystal deps install

crystal server-redis.cr &
