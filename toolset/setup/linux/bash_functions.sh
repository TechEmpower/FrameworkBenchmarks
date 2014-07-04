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

# Was there an error for the current dependency?
FW_dep_error=0
# Have we seen any errors?
FW_any_errors=0
fw_traperror () {
  depend=$1
  err=$2 # error status
  line=$3 # LINENO
  command="$4"
  FW_dep_error=1
  FW_any_errors=1

  echo "ERROR: ${depend}.sh at line $line - command '$command' exited with status: $err"
}

# Requires dependencies to come in order e.g. Nimrod before
# Jester, etc. Users should be know this 
# fairly well (e.g. you can't use Yaf without PHP)
fw_depends() {

  # Turn on errtrace (-E), so that our ERR
  # trap is passed on to any subshells
  set -E

  for depend in "$@"
  do
    depend=$(echo $depend | awk '{print tolower($0)}')
    echo Searching for $depend
    trap 'fw_traperror $depend $? $LINENO "$BASH_COMMAND"'  ERR
    retcode=0
    if [ -f ../toolset/setup/linux/systools/${depend}.sh ]; then
      echo Installing system tool: $depend 
      . ../toolset/setup/linux/systools/${depend}.sh
    elif [ -f ../toolset/setup/linux/languages/${depend}.sh ]; then
      echo Installing language: $depend 
      . ../toolset/setup/linux/languages/${depend}.sh
    elif [ -f ../toolset/setup/linux/webservers/${depend}.sh ]; then
      echo Installing webserver: $depend 
      . ../toolset/setup/linux/webservers/${depend}.sh
    elif [ -f ../toolset/setup/linux/frameworks/${depend}.sh ]; then
      echo Installing framework: $depend
      . ../toolset/setup/linux/frameworks/${depend}.sh
    else
      echo WARN: No installer found for $depend
      continue
    fi

    # For a sourced script to pass, all internal commands must return
    # non-zero. If you want to intentionally cause a failed install
    # message, just return a non-zero status from the sourced script
    if [ $FW_dep_error -ne 0 ]; then
      echo ERROR: $depend may not be installed properly

      # Reset variable for the next dependencies
      FW_dep_error=0
    else
      echo $depend is installed!
    fi
  done

  # Politely clean up our trap and trace
  set +E
  trap - ERR

  return $FW_any_errors
}

# Exits 0 if file or directory exists
fw_exists() {
  if [ -f $1 ] || [ -d $1 ]; then
    return 0
  else
    return 1
  fi 
}


