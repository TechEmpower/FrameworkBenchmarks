#!/bin/bash  
#
# Common functions for bash scripts.
#

# Set variables used for color displays.
if [ -z $TERM ] || [ $TERM == "dumb" ]
then  # non-interactive
    NORMAL=""
    BOLDCYAN=""
    BOLDYELLOW=""
    BOLDRED=""
else  # interactive
    NORMAL=$(tput sgr0)
    BOLDCYAN=$(tput setaf 6; tput bold)
    BOLDYELLOW=$(tput setaf 3; tput bold)
    BOLDRED=$(tput setaf 1; tput bold)
fi

# Color display functions.
function cyan {
    echo -e "$BOLDCYAN$*$NORMAL"
}

function red {
    echo -e "$BOLDRED$*$NORMAL"
}

function yellow {
    echo -e "$BOLDYELLOW$*$NORMAL"
}

# Displays informative message.
function information { cyan "$@"; }

# Displays warning message.
function warning { yellow "$@"; }

# Displays error message.
function error { red "ERROR: $@" 1>&2; }

# Displays error message and aborts.
function fail { [ $# -eq 0 ] || error "$@"; exit 1; }

# Retries a command a fixed number of times with a fixed linear backoff.
function retry {
    local command=$*
    local max_attempts=20 # Maximum number of attempts
    local sleep_time=30 # Seconds between attempts
    local attempt=1
    
    until $command; do
        ((attempt++))
        if [ $attempt -gt $max_attempts ]; then
            echo "The command has failed after $max_attempts attempts."
            return 1
        fi
        echo "The command failed. Attempt $attempt/$max_attempts will start in $sleep_time seconds."
        sleep $sleep_time
    done
}

# Detect CYGWIN.
function iscygwin {
    if [ "$(expr substr $(uname -s) 1 6)" == "CYGWIN" ]; then
        true
    else
        false
    fi
}

# Function used to invoke Windows batch files.
# It removes cygwinisms from the PATH and the environment first
# and does some argument pre-processing.
# It also seems to fix the space problem.
# Author: Igor Pechtchanski
# http://cygwin.com/ml/cygwin/2004-09/msg00150.html
function cywgin_cmd { 
    ( local c="`cygpath -w \"$1\"`";
    shift;
    local cmd=`cygpath -u $COMSPEC`;
    local args="";
    while [ $# != 0 ]; do
        if [ -f "$1" ]; then
            args="$args '`cygpath -w $1`'";
        else
            if [ -d "$1" ]; then
                args="$args '`cygpath -w $1 | sed 's@\\\\\$@@'`'";
            else
                args="$args '$1'";
            fi;
        fi;
        shift;
    done;
    PATH=`echo $PATH |
          tr : '\n' |
          egrep -vw '^(/usr/local/bin|/usr/bin|/bin|/usr/X11R6/bin)$' |
          tr '\n' :`;
    unset BASH_ENV COLORTERM CYGWIN DISPLAY HISTCONTROL MAKE_MODE;
    unset MANPATH PKG_CONFIG_PATH PS1 PWD SHLVL TERM USER _;
    unset CVS CVSROOT CVS_RSH GEN_HOME GROOVY_HOME TOMCAT_DIR;
    eval $cmd /c "$c" $args
    )
}

# Runs a bash script. Under Cygwin, ignores Windows end-of-line characters.
function run_bash {
    if iscygwin
    then
        BASH_OPTIONS='-o igncr'
    else
        BASH_OPTIONS=''
    fi
    bash $BASH_OPTIONS $@
}

# Returns value of property stored in $prop on JSON object stored in $json.
# Author: Carlos Justiniano
# https://gist.github.com/cjus/1047794
function jsonval {
    local temp=`echo $json | sed 's/\\\\\//\//g' | sed 's/[{}]//g' | awk -v k="text" '{n=split($0,a,","); for (i=1; i<=n; i++) print a[i]}' | sed 's/\"\:\"/\|/g' | sed 's/[\,]/ /g' | sed 's/\"//g' | grep -w $prop | awk -F": " '{print $2}'`
    echo ${temp##*|}
}

# Runs a script on a remote Linux host.
function run_remote_script {
    local description=$1
    local username=$2
    local host=$3
    local private_key_file=$4
    local script_file=$5
    local log_file=$6
    echo "$description"
    echo "Running script $script_file on $host. To watch the progress, run in another terminal:"
    echo "tail -f $log_file"
    tr -d '\r' < $script_file | ssh $username@$host -i "$private_key_file" -o StrictHostKeyChecking=no -o BatchMode=yes "bash -s" &>$log_file
}

# Reboots a Linux host.
function reboot_linux_host {
    local username=$1
    local host=$2
    local private_key_file=$3
    # Reboot.
    echo "Rebooting $host"
    retry ssh $username@$host -i "$private_key_file" -o StrictHostKeyChecking=no -o BatchMode=yes "sudo reboot" || fail "Couldn't reboot $host."
    # Wait for reboot to start.
    sleep 15s
    # Reconnect.
    echo "Verifying $host availability"
    retry ssh $username@$host -i "$private_key_file" -o StrictHostKeyChecking=no -o BatchMode=yes -o ConnectionAttempts=5 -o ConnectTimeout=60 "last reboot | head -1" || fail "Couldn't verify $host availability."
    echo "$host rebooted"
}

# Uploads a file to a remote Linux host.
function upload_file {
    local file_to_upload=$1
    local remote_user=$2
    local remote_host=$3
    local target_directory=$4
    local private_key_file=$5
    echo "Uploading file $file_to_upload to $remote_host:$target_directory"
    retry scp -o StrictHostKeyChecking=no -i "$private_key_file" "$file_to_upload" "$remote_user@$remote_host:$target_directory" || fail "Error uploading file."
}

# Downloads a file from a remote Linux host.
function download_file {
    local remote_user=$1
    local remote_host=$2
    local remote_file=$3
    local target_file=$4
    local private_key_file=$5
    echo "Downloading $remote_host:$remote_file"
    retry scp -o StrictHostKeyChecking=no -i "$private_key_file" "$remote_user@$remote_host:$remote_file" "$target_file" || fail "Error downloading file."
}
