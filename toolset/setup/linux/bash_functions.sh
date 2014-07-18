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
  tar xvf "$@"
}

fw_unzip() {
  unzip "$@"
}

# Was there an error for the current dependency?
FW_dep_error=0
# Have we seen any errors?
FW_any_errors=0
fw_traperror () {
  depend=$1      # Dependency being installed
  err=$2         # error status
  line=$3        # Current line
  command="$4"   # Bash command
  IFS=':' read -a funcstack <<< "$5" # Stack (function names)
  IFS=':' read -a bashstack <<< "$6" # Stack (file names)
  IFS=':' read -a linestack <<< "$7" # Stack (line numbers)
  FW_dep_error=1
  FW_any_errors=1

  wd=$(pwd)
  relative_wd=\$FWROOT${wd#$FWROOT}
  
  echo "ERROR: $(echo ${bashstack[1]#$FWROOT}): Command '$command' exited with status $err (dependency=$depend) (cwd=$relative_wd)"
  #echo "  Function stack    : ${funcstack[@]}"
  #echo "  Bash source stack : ${bashstack[@]}"
  #echo "  Bash line stack   : ${linestack[@]}"
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
    trap 'fw_traperror $depend $? $LINENO "$BASH_COMMAND" $(printf ":%s" ${FUNCNAME[@]}) $(printf ":%s" ${BASH_SOURCE[@]}) $(printf ":%s" ${BASH_LINENO[@]})'  ERR
    retcode=0

    wd=$(pwd)
    relative_wd=\$FWROOT${wd#$FWROOT}

    if [ -f $FWROOT/toolset/setup/linux/systools/${depend}.sh ]; then
      echo Installing system tool: $depend in $relative_wd
      . $FWROOT/toolset/setup/linux/systools/${depend}.sh
    elif [ -f $FWROOT/toolset/setup/linux/languages/${depend}.sh ]; then
      echo Installing language: $depend in $relative_wd
      . $FWROOT/toolset/setup/linux/languages/${depend}.sh
    elif [ -f $FWROOT/toolset/setup/linux/webservers/${depend}.sh ]; then
      echo Installing webserver: $depend in $relative_wd
      . $FWROOT/toolset/setup/linux/webservers/${depend}.sh
    elif [ -f $FWROOT/toolset/setup/linux/frameworks/${depend}.sh ]; then
      echo Installing framework: $depend in $relative_wd
      . $FWROOT/toolset/setup/linux/frameworks/${depend}.sh
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

# Echo's 0 if file or directory exists
# To be used with or || blocks, avoids triggering our ERR 
# trap with a return 1 statement
fw_exists() {
  if [ -f $1 ] || [ -d $1 ]; then
    echo 0
  else
    echo 1
  fi 
}


