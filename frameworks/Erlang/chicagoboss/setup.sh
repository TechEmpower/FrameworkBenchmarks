#!/bin/bash

fw_depends erlang

rm -rf deps/* ebin/*
rebar3 get-deps
rebar3 compile

erl -pa ebin deps/*/ebin +sbwt very_long +swt very_low -config boss -s boss -sname chicagoboss -noshell -detached
