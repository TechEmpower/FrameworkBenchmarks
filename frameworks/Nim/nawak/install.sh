#!/bin/bash

fw_depends nim zeromq mongrel2

export PATH="$IROOT/nim/bin:$PATH"

if ! nimble path nawak > /dev/null 2>&1; then
  pushd $IROOT
  if [ ! -d nawak ]; then
    git clone https://github.com/idlewan/nawak.git
  fi
  cd nawak
  git fetch
  git checkout b34b0b5077541ae9671957452a70e2578894d3a8
  nimble update
  nimble install
  popd
fi
