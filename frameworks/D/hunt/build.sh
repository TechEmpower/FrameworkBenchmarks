#!/bin/bash

rm -rf picohttpparser
git clone https://github.com/h2o/picohttpparser.git
cp patches/Makefile picohttpparser
cd picohttpparser
make package


rm -rf http-parser
git clone https://github.com/nodejs/http-parser.git
cd http-parser
make package