#!/bin/bash

sed -i 's|"benchmarkdbpass", ".*", 3306|"benchmarkdbpass", "'"${DBHOST}"'", 3306|g' src/hello_world_app.erl

fw_depends erlang

rm -rf deps/* ebin/*
rebar get-deps
rebar compile

erl -pa ebin deps/*/ebin +sbwt very_long +swt very_low -s hello_world -noshell -detached
