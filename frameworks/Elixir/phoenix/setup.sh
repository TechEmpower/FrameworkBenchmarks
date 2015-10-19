#!/bin/bash

fw_depends elixir

sed -i 's|localhost|'${DBHOST}'|g' config/prod.exs

rm -rf _build deps rel

MIX_ENV=prod
export MIX_ENV
mix local.hex --force
mix deps.get --force
mix compile --force

elixir --detached -S mix phoenix.server
