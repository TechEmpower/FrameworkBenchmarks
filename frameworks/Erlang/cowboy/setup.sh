#!/bin/bash

# Caution: "line 11 has flaw." reported by 'blee@techempower.com'
#   Below sed command can cause travis-ci test failing.
#   Please be aware of testing this on local machine.
#   If you perform a local test, please apply inverse of   
#   below command. (hint: ${DBHOST} value must be rollback to
#   localhost.
#
#   I will fix this problem later with better solution. 
#
sed -i 's|"benchmarkdbpass", ".*", 3306|"benchmarkdbpass", "'"${DBHOST}"'", 3306|g' src/hello_world_app.erl

fw_depends erlang

rm -rf deps/* ebin/*
rebar get-deps
rebar compile

erl -pa ebin deps/*/ebin +sbwt very_long +swt very_low -s hello_world -noshell -detached
