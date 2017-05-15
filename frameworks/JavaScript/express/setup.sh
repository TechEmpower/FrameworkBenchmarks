#!/bin/bash

fw_depends nodejs

# install dependencies
npm install
# run app
NODE_ENV=production node app &
