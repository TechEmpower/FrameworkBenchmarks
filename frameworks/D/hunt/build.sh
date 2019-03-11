#!/bin/bash

rm -rf http-parser
git clone https://github.com/nodejs/http-parser.git
cd http-parser
make package
