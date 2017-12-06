#!/bin/bash

fw_depends erlang mysql

rm -rf deps/* ebin/*
rebar3 get-deps
rebar3 compile

erl +K true +sbwt very_long +swt very_low -pa ebin deps/*/ebin -boot start_sasl -config priv/app.config -s mochiweb_bench_app
