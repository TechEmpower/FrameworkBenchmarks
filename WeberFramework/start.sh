#!/usr/bin/env sh

if [ ! -f deps ]; then
  mix deps.get && mix compile
fi

exec elixir --detached -S mix run --no-halt
