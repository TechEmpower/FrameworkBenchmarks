#!/bin/bash

fw_depends elixir

sed -i 's|localhost|'${DBHOST}'|g' config/prod.exs

rm -rf _build deps

export MIX_ENV=prod
mix local.hex --force
mix deps.get --force
mix compile --force

elixir --detached -S mix phoenix.server
