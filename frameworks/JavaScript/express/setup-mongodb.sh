#!/bin/bash

fw_depends nodejs mongodb

# install dependencies
npm install
# run app
NODE_ENV=production node mongodb-app &
