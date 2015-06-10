#!/bin/sh

cd "$(dirname "$0")"

FROM=/tmp/tfb-temporary-mysql.log
FINAL_DESTINATION=$1
TEST_NAME=$2

if [ -f $FROM ]
then
    cp /tmp/tfb-temporary-mysql.log $FINAL_DESTINATION
    echo "Successfully moved MySQL temporary log file for $TEST_NAME" 
else
	echo "Expected MySQL log file at $FROM, but found none, no files copied"
	return 1
fi

return 0