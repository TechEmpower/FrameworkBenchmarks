#!/bin/bash

fw_depends nodejs

# install dependencies
npm install --no-bin-links
# run app
NODE_ENV=production node app &
