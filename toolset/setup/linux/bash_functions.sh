#!/bin/bash

# If you are running a large number of installs back to back 
# (e.g. for FwBm development), then setting this variable 
# will cause apt-get and wget to use your proxy server. If 
# your proxy server has a cache for static content, this can 
# save you quite a lot of download time
# export http_proxy=http://10.0.1.0:3128

fw_get () {
  wget --no-check-certificate --trust-server-names "$@"
}

fw_untar() {
  # tar xzf "$@"
  tar xvzf "$@"
}

fw_unzip() {
  unzip "$@"
}

# Requires dependencies to come in order e.g. Nimrod before
# Jester, etc. Users should be know this 
# fairly well (e.g. you can't use Yaf without PHP)
fw_depends() {
  for depend in "$@"
  do
    echo Searching for $depend
    if [ -f ../toolset/setup/linux/systools/${depend}.sh ]; then
      echo Installing system tool: $depend 
      bash ../toolset/setup/linux/systools/${depend}.sh
    fi
    if [ -f ../toolset/setup/linux/languages/${depend}.sh ]; then
      echo Installing language: $depend 
      bash ../toolset/setup/linux/languages/${depend}.sh
    fi
    if [ -f ../toolset/setup/linux/webservers/${depend}.sh ]; then
      echo Installing webserver: $depend 
      bash ../toolset/setup/linux/webservers/${depend}.sh
    fi
    if [ -f ../toolset/setup/linux/frameworks/${depend}.sh ]; then
      echo Installing framework: $depend
      bash ../toolset/setup/linux/frameworks/${depend}.sh
    fi
  done  
}

# Exits 0 if file or directory exists
fw_exists() {
  if [ -f $1 ] || [ -d $1 ]; then
    return 0
  else
    return 1
  fi 
}


