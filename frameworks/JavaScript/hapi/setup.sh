#!/bin/bash

fw_depends mongodb postgresql mysql nodejs

npm install
node app &
