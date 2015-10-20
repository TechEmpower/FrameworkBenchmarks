#!/bin/bash

fw_depends elixir

rm -rf _build deps rel

MIX_ENV=prod
export MIX_ENV
mix local.hex --force
mix deps.get --force
mix compile --force

elixir --detached --no-halt -S mix
