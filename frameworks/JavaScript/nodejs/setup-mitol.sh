#!/bin/bash

fw_depends mongodb mysql postgresql nodejs

npm install

# Mitol installation
mkdir -p node_modules/mns & mkdir -p tmp
wget https://github.com/Helidium/Mitol/archive/v0.0.1.tar.gz -P tmp
tar -xzvf tmp/v0.0.1.tar.gz -C tmp
make -C tmp/Mitol-0.0.1/node
cp tmp/Mitol-0.0.1/node/dist/* node_modules/mns
rm -R tmp

node app-mitol.js &
