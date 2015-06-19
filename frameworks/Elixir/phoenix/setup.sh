#!/bin/bash

sed -i 's|db_host: "localhost",|db_host: "${DBHOST}",|g' config/config.exs

fw_depends elixir

rm -rf _build deps rel

MIX_ENV=prod
export MIX_ENV
mix local.hex --force
mix deps.get --force
mix compile --force

elixir --detached -S mix phoenix.server
