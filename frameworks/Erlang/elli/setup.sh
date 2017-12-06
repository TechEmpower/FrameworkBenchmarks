#!/bin/bash

fw_depends erlang mysql

rm -rf deps/* ebin/*
rebar3 get-deps
rebar3 compile

erl -pa ebin deps/*/ebin +sbwt very_long +swt very_low -s elli_bench -noshell -detached
