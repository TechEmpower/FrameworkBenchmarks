#!/bin/sh
#
# Author: Tyrone Hattingh
# web2py issue: http://code.google.com/p/web2py/issues/detail?id=867
#
# 1) vi web2py-scheduler
# 2) Paste in the above
# 3) Add in the following 2 lines in the web2py-scheduler file
#    (required for centos i believe):
#
#  # chkconfig: 2345 90 10
#  # description: web2py-scheduler
#
# 4) make it executable with
#
#  chmod 755 web2py-scheduler
#
# 5) add it to startup with
#
#  chkconfig --add web2py-scheduler
#

DAEMON=/usr/local/bin/python
PARAMETERS="/var/www/html/web2py/web2py.py -K init"
LOGFILE=/var/log/web2py-scheduler.log

start() {
    echo -n "starting up $DAEMON"
    RUN=`$DAEMON $PARAMETERS > $LOGFILE 2>&1`
    if [ "$?" -eq 0 ]; then
        echo " Done."
    else
        echo " FAILED."
    fi
}
stop() {
    killall $DAEMON
}
status() {
    killall -0 $DAEMON
    if [ "$?" -eq 0 ]; then
        echo "Running."
    else
        echo "Not Running."
    fi
}
case "$1" in
    start)
    start
    ;;
    restart)
    stop
    sleep 2
    start
    ;;
    stop)
    stop
    ;;
    status)
    status
    ;;
    *)
    echo "usage : $0 start|restart|stop|status"
    ;;
esac
exit 0
