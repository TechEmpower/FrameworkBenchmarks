#!/bin/bash

export DB_HOST={database_host}

set -x
export DEBIAN_FRONTEND=noninteractive

##############################
# Prerequisites
##############################
sudo apt-get -y update

# WARNING: DONT PUT A SPACE AFTER ANY BACKSLASH OR APT WILL BREAK
# Dpkg::Options avoid hangs on Travis-CI, doesn't affect clean systems
sudo apt-get -y install -o Dpkg::Options::="--force-confdef" -o Dpkg::Options::="--force-confold" \
    build-essential git libev-dev libpq-dev libreadline6-dev

sudo sh -c "echo '*               -    nofile          65535' >> /etc/security/limits.conf"

##############################
# wrk
##############################

git clone https://github.com/wg/wrk.git
cd wrk
git checkout 205a1960c8b8de5f500bb143863ae293456b7add
make
sudo cp wrk /usr/local/bin
cd ~

#############################
# pipeline.lua
#############################
cat << EOF | tee pipeline.lua
init = function(args)
  wrk.init(args)
  local r = {}
  local depth = tonumber(args[1]) or 1
  for i=1,depth do
    r[i] = wrk.format()
  end
  req = table.concat(r)
end

request = function()
  return req
end
EOF
