#!/bin/bash

fw_depends nim nginx

export PATH="$IROOT/nim/bin:$PATH"

if ! nimble path jester > /dev/null 2>&1; then
  pushd $IROOT
  if [ ! -d jester ]; then
    git clone https://github.com/dom96/jester.git
  fi
  cd jester
  git fetch origin
  git checkout da9e3a73ecac51494430dce2a8387e5f0e32f968
  nimble update
  nimble install
  popd
fi
