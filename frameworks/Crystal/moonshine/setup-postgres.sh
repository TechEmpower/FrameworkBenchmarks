#!/bin/bash

curl http://dist.crystal-lang.org/apt/setup.sh | sudo bash

sudo apt-get install crystal

crystal deps install

crystal server-postgres.cr -- $TFB_SERVER_HOST &
