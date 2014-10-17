#!/bin/bash

RETCODE=$(fw_exists rust.installed)
[ ! "$RETCODE" == 0 ] || { return 0; }

curl -s https://static.rust-lang.org/rustup.sh | sudo sh

touch rust.installed
