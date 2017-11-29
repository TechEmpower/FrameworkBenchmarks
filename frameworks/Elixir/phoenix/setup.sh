#!/bin/bash

fw_depends postgresql elixir

rm -rf _build deps

export MIX_ENV=prod
mix local.hex --force
mix local.rebar --force
mix release.init
mix deps.get --force --only prod
mix compile --force
mix release
ERL_AFLAGS="+K true +sbwt very_long +swt very_low" _build/prod/rel/hello/bin/hello foreground
#elixir --erl "+K true +sbwt very_long +swt very_low" --detached -S mix phoenix.server
