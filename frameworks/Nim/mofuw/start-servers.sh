#!/bin/bash

nim c -d:release --threads:on techempower.nim

./techempower

wait
