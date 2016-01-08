#!/bin/bash

fw_depends elixir

# 
# Caution: "line 20 has flaw."
#
#   Below sed command will cause another travis-ci test failing 
#   if a person is executed local test. I think that line is original author's mistake.
#   The logic is correct, but below command will make permanent change
#   to 'config/proud.exs'. For this reason, if a tester of this framework is 
#   going to execute this on local machine, then push this to remote
#   it will fail travis-ci test.
#
#   For now, I keep this original code and leave this comment. 
#   I will fix this problem later with better solution.
#
#   blee@techempower.com
#
sed -i 's|localhost|'${DBHOST}'|g' config/prod.exs

rm -rf _build deps

export MIX_ENV=prod
mix local.hex --force
mix deps.get --force
mix compile --force

elixir --detached -S mix phoenix.server
