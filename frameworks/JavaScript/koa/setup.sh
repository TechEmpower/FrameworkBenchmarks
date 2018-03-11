#!/bin/bash

fw_depends mongodb postgresql mysql nodejs

npm install --no-bin-links
node app &
