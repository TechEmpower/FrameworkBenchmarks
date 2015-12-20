#!/bin/bash

echo "@@@@@@@ Crystal Installation @@@@@@@"
curl http://dist.crystal-lang.org/apt/setup.sh | sudo bash

sudo apt-get install crystal

echo "@@@@@@@ Dependency Setup @@@@@@@"

crystal deps install

echo "@@@@@@@ Execute Moonshine Server @@@@@@@"
echo "@@@@@@@ CHECK ENVIRONMENT VARIABLE "
echo $TFB_SERVER_HOST
echo "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"

crystal server-postgres.cr -- $TFB_SERVER_HOST &
