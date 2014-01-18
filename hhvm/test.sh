#!/bin/bash

for u in `cat URLs`; do echo "working on : $u"; perl -le print = x 50; echo; curl $u; echo; echo; done
