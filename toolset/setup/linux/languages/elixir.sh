#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/elixir.installed)
[ ! "$RETCODE" == 0 ] || { return 0; }

fw_depends erlang
sudo apt-get install -y elixir

touch ${IROOT}/elixir.installed
