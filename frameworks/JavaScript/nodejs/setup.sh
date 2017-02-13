#!/bin/bash

fw_depends mongodb mysql postgresql nodejs

npm install
node app.js &
