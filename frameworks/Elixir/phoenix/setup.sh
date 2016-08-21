#!/bin/bash

fw_depends elixir

sed -i 's|localhost|'${DBHOST}'|g' config/prod.exs

rm -rf _build deps

export MIX_ENV=prod
mix local.hex --force
mix local.rebar --force
mix deps.get --force --only prod
mix compile --force

elixir --erl "+K true" --detached  -pa _build/prod/consolidated -S mix phoenix.server
