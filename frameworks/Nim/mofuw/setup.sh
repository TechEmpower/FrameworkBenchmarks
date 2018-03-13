#!/bin/bash

fw_depends nim

git clone https://github.com/2vg/mofuw mofuw

cd mofuw/tests/techempower

nim c -d:release --threads:on --path:"../../src" techempower.nim

./techempower &
