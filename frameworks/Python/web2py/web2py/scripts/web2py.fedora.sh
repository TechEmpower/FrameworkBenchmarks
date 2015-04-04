#!/bin/bash
#
#       /etc/rc.d/init.d/web2pyd
#
# Starts the Web2py Daemon on Fedora (Red Hat Linux)
#
# To execute automatically at startup
#
#    sudo chkconfig --add web2pyd
#
# chkconfig: 2345 90 10
# description: Web2py Daemon
# processname: web2pyd
# pidfile: /var/lock/subsys/web2pyd

source /etc/rc.d/init.d/functions

RETVAL=0
NAME=web2pyd
DESC="Web2py Daemon"
DAEMON_DIR="/usr/lib/web2py"
ADMINPASS="admin"
#ADMINPASS="\<recycle\>"
PIDFILE=/var/run/$NAME.pid
PORT=8001
PYTHON=python

cd $DAEMON_DIR

start() {
        echo -n $"Starting $DESC ($NAME): "
        daemon --check $NAME $PYTHON $DAEMON_DIR/web2py.py -Q --nogui -a $ADMINPASS -d $PIDFILE -p $PORT &
        RETVAL=$?
        if [ $RETVAL -eq 0 ]; then
                touch /var/lock/subsys/$NAME
        fi
        echo
        return $RETVAL
}

stop() {
        echo -n $"Shutting down $DESC ($NAME): "
        killproc -p "$PIDFILE" -d 3 "$NAME"
        echo
        if [ $RETVAL -eq 0 ]; then
                rm -f /var/lock/subsys/$NAME
                rm -f $PIDFILE
        fi
        return $RETVAL
}

restart() {
        stop
        start
}

status() {
        if [ -r "$PIDFILE" ]; then
                pid=`cat $PIDFILE`
        fi
        if [ $pid ]; then
                echo "$NAME (pid $pid) is running..."
        else
                echo "$NAME is stopped"
        fi
}

case "$1" in
        start)              start;;
        stop)               stop;;
        status)             status;;
        restart)            restart;;
        condrestart)        [ -e /var/lock/subsys/$NAME ] && restart
                            RETVAL=$?
                            ;;
        *)                  echo $"Usage: $0 {start|stop|restart|condrestart|status}"
                            RETVAL=1
                            ;;
esac

exit $RETVAL
