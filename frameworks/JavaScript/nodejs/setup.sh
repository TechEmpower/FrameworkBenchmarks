#!/bin/bash

fw_depends mongodb mysql postgresql nodejs

npm install --no-bin-links
node app.js &
