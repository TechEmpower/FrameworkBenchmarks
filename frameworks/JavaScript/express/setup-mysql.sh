#!/bin/bash

fw_depends nodejs mysql

# install dependencies
npm install
# run app
NODE_ENV=production node mysql-app &
