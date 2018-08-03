#!/bin/bash

fw_depends elixir

rm -rf _build deps

export MIX_ENV=prod
mix local.hex --force
mix local.rebar --force
mix deps.get --force --only prod
mix compile --force

elixir --erl "+K true +sbwt very_long +swt very_low" --detached --no-halt -S mix
