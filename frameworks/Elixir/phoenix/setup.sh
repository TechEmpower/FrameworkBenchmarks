#!/bin/bash

source $IROOT/elixir.installed

sed -i 's|db_host: "localhost",|db_host: "${DBHOST}",|g' config/config.exs

rm -rf _build deps

mix local.hex --force
mix local.rebar --force
mix deps.get --force

MIX_ENV=prod mix compile.protocols --force
MIX_ENV=prod elixir --detached -pa _build/$MIX_ENV/consolidated -S mix phoenix.server

