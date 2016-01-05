#!/bin/bash

sed -i 's|"benchmarkdbpass", ".*", 3306|"benchmarkdbpass", "'"${DBHOST}"'", 3306|g' src/misultin_bench_sup.erl

fw_depends erlang

rm -rf deps/* ebin/*
rebar get-deps
rebar compile

erl +K true +sbwt very_long +swt very_low -pa ebin deps/*/ebin -boot start_sasl -config priv/app.config -s misultin_bench_app -noshell -detached
