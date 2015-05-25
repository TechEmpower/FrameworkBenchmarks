#!/bin/bash
set -ex

RETCODE=$(fw_exists ${IROOT}/crystal.installed)
[ ! "$RETCODE" == 0 ] || { return 0; }

sudo apt-key adv --keyserver keys.gnupg.net --recv-keys 09617FD37CC06B54
echo "deb http://dist.crystal-lang.org/apt crystal main" | sudo tee /etc/apt/sources.list.d/crystal.list

sudo apt-get update

sudo apt-get install -y crystal

touch ${IROOT}/crystal.installed
