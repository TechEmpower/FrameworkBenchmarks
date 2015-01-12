#!/bin/bash
export REBAR_HOME=$IROOT/rebar

sed -i 's|"benchmarkdbpass", ".*", 3306|"benchmarkdbpass", "'"${DBHOST}"'", 3306|g' src/hello_world_app.erl

rm -rf deps/*
$REBAR_HOME/rebar get-deps
$REBAR_HOME/rebar compile
erl -pa ebin deps/*/ebin +sbwt very_long +swt very_low -s hello_world -noshell -detached