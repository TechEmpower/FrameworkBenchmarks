#!/usr/bin/env sh
################################################################################
#
# Cake is a shell script for invoking CakePHP shell commands
#
# CakePHP(tm) :  Rapid Development Framework (https://cakephp.org)
# Copyright (c) Cake Software Foundation, Inc. (https://cakefoundation.org)
#
# Licensed under The MIT License
# For full copyright and license information, please see the LICENSE.txt
# Redistributions of files must retain the above copyright notice.
#
# @copyright     Copyright (c) Cake Software Foundation, Inc. (https://cakefoundation.org)
# @link          https://cakephp.org CakePHP(tm) Project
# @since         1.2.0
# @license       https://opensource.org/licenses/mit-license.php MIT License
#
################################################################################

# Canonicalize by following every symlink of the given name recursively
canonicalize() {
    NAME="$1"
    if [ -f "$NAME" ]
    then
        DIR=$(dirname -- "$NAME")
        NAME=$(cd -P "$DIR" > /dev/null && pwd -P)/$(basename -- "$NAME")
    fi
    while [ -h "$NAME" ]; do
        DIR=$(dirname -- "$NAME")
        SYM=$(readlink "$NAME")
        NAME=$(cd "$DIR" > /dev/null && cd $(dirname -- "$SYM") > /dev/null && pwd)/$(basename -- "$SYM")
    done
    echo "$NAME"
}

# Find a CLI version of PHP
findCliPhp() {
    for TESTEXEC in php php-cli /usr/local/bin/php
    do
        SAPI=`echo "<?= PHP_SAPI ?>" | $TESTEXEC 2>/dev/null`
        if [ "$SAPI" = "cli" ]
        then
            echo $TESTEXEC
            return
        fi
    done
    echo "Failed to find a CLI version of PHP; falling back to system standard php executable" >&2
    echo "php";
}

# If current path is a symlink, resolve to real path
realname="$0"
if [ -L "$realname" ] 
then
	realname=$(readlink -f "$0")
fi

CONSOLE=$(dirname -- "$(canonicalize "$realname")")
APP=$(dirname "$CONSOLE")

# If your CLI PHP is somewhere that this doesn't find, you can define a PHP environment
# variable with the correct path in it.
if [ -z "$PHP" ]
then
    PHP=$(findCliPhp)
fi

if [ $(basename $realname) != 'cake' ]
then
    exec $PHP "$CONSOLE"/cake.php $(basename $realname) "$@"
else
    exec $PHP "$CONSOLE"/cake.php "$@"
fi

exit
