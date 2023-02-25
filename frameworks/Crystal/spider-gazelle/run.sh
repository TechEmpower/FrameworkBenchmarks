#!/bin/bash

bin/app -w $(nproc --all) -b 0.0.0.0 -p 8080

wait
