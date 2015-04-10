#!/bin/bash

source $IROOT/erlang.installed
source $IROOT/rebar.installed

sed -i 's|"benchmarkdbpass", ".*", 3306|"benchmarkdbpass", "'"${DBHOST}"'", 3306|g' src/elli_bench_sup.erl

rm -rf deps/*
rebar get-deps
rebar compile
erl -pa ebin deps/*/ebin +sbwt very_long +swt very_low -s elli_bench -noshell -detached
