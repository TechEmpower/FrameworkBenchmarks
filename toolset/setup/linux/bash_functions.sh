#!/bin/bash

# If you are running a large number of installs back to back 
# (e.g. for FwBm development), then setting this variable 
# will cause apt-get and wget to use your proxy server. If 
# your proxy server has a cache for static content, this can 
# save you quite a lot of download time
# export http_proxy=http://10.0.1.0:3128

fw_get () {
  # Start a background process to print a dot every
  # 30 seconds (avoids travis-ci 10min timeout)
  while :;do sleep 30; echo -n .;done &

  # -no-verbose disables the big progress bars but keeps
  # other basic info
  #wget --no-verbose --no-check-certificate \
  #  --trust-server-names "$@"
  # DEPRECATED - older versions of wget use SSLv3 for handshaking
  # and therefore don't work (Ubuntu12, for instance).
  # Use curl instead (-s means silent; -L means follow 3xx responses)
  curl -sL "$@"

  # Ensure the background job is killed if we are
  kill $!; trap 'kill $!' SIGTERM
}

fw_untar() {
  echo "Running 'tar xf $@'...please wait"
  tar xf "$@"
  echo "Removing compressed tar file"
  
  # use -f to avoid printing errors if they gave additional arguments
  rm -f "$@"
}

fw_unzip() {
  echo "Running 'unzip $@'...please wait"
  unzip -o -q "$@"
  echo "Removing compressed zip file"
  # use -f to avoid printing errors if they gave additional arguments
  rm -f "$@"
}

# Download *.deb file and install into IROOT 
# Cautions:
#   Without using sudo,
#   Does not download dependant packages.
#   script will be stuck and will not make progress. (e.g: CSharp/nancy)
# Example: fw_apt_to_iroot <package> [<directory>]
fw_apt_to_iroot() {
  DIR=${2:-$1}
  echo "Downloading $1 to $IROOT"
  sudo apt-get download $1
  echo "Extracting $1 to $DIR"
  sudo dpkg-deb -x $1*.deb "$IROOT/$DIR" && sudo rm $1*.deb
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

    # Ensure we are inside the installer root for this framework
    pushd $IROOT
    wd=$(pwd)
    relative_wd=\$FWROOT${wd#$FWROOT}

    # Check that the prerequisites have been loaded
    RETCODE=$(fw_exists ${IROOT}/prerequisites.installed)
    [ "$RETCODE" == 0 ] || { \
      # Load environment variables
      echo Installing prerequisites
      source $FWROOT/toolset/setup/linux/prerequisites.sh
      touch $IROOT/prerequisites.installed; }

    # Find and run the installer.sh file for this dependency
    # Turn on some bash options before sourcing: 
    #   - (x) errtrace : Print commands before they are run
    # Note: A shebang is just a comment when you source a script, 
    #       so if you need to modify the default options use  
    #       `set -e` instead of `#!/bin/bash -e`
    if [ -f $FWROOT/toolset/setup/linux/systools/${depend}.sh ]; then
      echo Installing system tool: $depend in $relative_wd
      set -x
      . $FWROOT/toolset/setup/linux/systools/${depend}.sh
    elif [ -f $FWROOT/toolset/setup/linux/languages/${depend}.sh ]; then
      echo Installing language: $depend in $relative_wd
      set -x
      . $FWROOT/toolset/setup/linux/languages/${depend}.sh
    elif [ -f $FWROOT/toolset/setup/linux/webservers/${depend}.sh ]; then
      echo Installing webserver: $depend in $relative_wd
      set -x
      . $FWROOT/toolset/setup/linux/webservers/${depend}.sh
    elif [ -f $FWROOT/toolset/setup/linux/frameworks/${depend}.sh ]; then
      echo Installing framework: $depend in $relative_wd
      set -x
      . $FWROOT/toolset/setup/linux/frameworks/${depend}.sh
    else
      echo WARN: No installer found for $depend
      # Return whence you came.
      popd
      continue
    fi
    set +x

    # Return whence you came.
    popd

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
