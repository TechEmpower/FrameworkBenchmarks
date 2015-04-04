#! /bin/sh
### BEGIN INIT INFO
# startup script for Ubuntu and Debian Linux servers
#
# To use this file
# cp ubuntu.sh /etc/init.d/web2py
#
# To automatitcally start at reboot
# sudo update-rc.d web2py defaults
#
# Provides:          web2py
# Required-Start:    $local_fs $remote_fs
# Required-Stop:     $local_fs $remote_fs
# Default-Start:     2 3 4 5
# Default-Stop:      S 0 1 6
# Short-Description: web2py initscript
# Description:       This file starts up the web2py server.
### END INIT INFO

# Author: Mark Moore <mark.moore@fonjax.com>

PATH=/usr/sbin:/usr/bin:/sbin:/bin
DESC="Web Framework"
NAME=web2py
PIDDIR=/var/run/$NAME
PIDFILE=$PIDDIR/$NAME.pid
SCRIPTNAME=/etc/init.d/$NAME
DAEMON=/usr/bin/python
DAEMON_DIR=/usr/lib/$NAME
DAEMON_ARGS="web2py.py --password=<recycle> --pid_filename=$PIDFILE"
DAEMON_USER=web2py

# Exit if the package is not installed
[ -x "$DAEMON" ] || exit 0

# Read configuration variable file if it is present
[ -r /etc/default/$NAME ] && . /etc/default/$NAME

# Load the VERBOSE setting and other rcS variables
[ -f /etc/default/rcS ] && . /etc/default/rcS

# Define LSB log_* functions.
# Depend on lsb-base (>= 3.0-6) to ensure that this file is present.
. /lib/lsb/init-functions

#
# Function that starts the daemon/service
#
do_start()
{
        # Return
        #   0 if daemon has been started
        #   1 if daemon was already running
        #   2 if daemon could not be started

        # The PIDDIR should normally be created during installation. This
        # fixes things just in case.
        [ -d $PIDDIR ] || mkdir -p $PIDDIR
        [ -n "$DAEMON_USER" ] && chown --recursive $DAEMON_USER $PIDDIR

        # Check to see if the daemon is already running.
        start-stop-daemon --stop --test --quiet --pidfile $PIDFILE \
                && return 1

        start-stop-daemon --start --quiet --pidfile $PIDFILE \
                ${DAEMON_USER:+--chuid $DAEMON_USER} --chdir $DAEMON_DIR \
                --background --exec $DAEMON -- $DAEMON_ARGS \
                || return 2

        return 0;
}

#
# Function that stops the daemon/service
#
do_stop()
{
        # Return
        #   0 if daemon has been stopped
        #   1 if daemon was already stopped
        #   2 if daemon could not be stopped
        #   other if a failure occurred

        start-stop-daemon --stop --quiet --retry=TERM/30/KILL/5 --pidfile $PIDFILE
        RETVAL=$?
        # Many daemons don't delete their pidfiles when they exit.
        rm -f $PIDFILE
        return "$RETVAL"
}

#
# Function that restarts the daemon/service
#
do_restart()
{
        # Return
        #   0 if daemon was (re-)started
        #   1 if daemon was not strated or re-started

        do_stop
        case "$?" in
                0|1)
                        do_start
                        case "$?" in
                                0) RETVAL=0 ;;
                                1) RETVAL=1 ;; # Old process is still running
                                *) RETVAL=1 ;; # Failed to start
                        esac
                        ;;
                *) RETVAL=1 ;; # Failed to stop
        esac

        return "$RETVAL"
}

#
# Function that sends a SIGHUP to the daemon/service
#
do_reload() {
        #
        # If the daemon can reload its configuration without
        # restarting (for example, when it is sent a SIGHUP),
        # then implement that here.
        #
        start-stop-daemon --stop --signal 1 --quiet --pidfile $PIDFILE
        return 0
}

#
# Function that queries the status of the daemon/service
#
do_status()
{
        # Return
        #   0 if daemon is responding and OK
        #   1 if daemon is not responding, but PIDFILE exists
        #   2 if daemon is not responding, but LOCKFILE exists
        #   3 if deamon is not running
        #   4 if daemon status is unknown

        # Check to see if the daemon is already running.
        start-stop-daemon --stop --test --quiet --pidfile $PIDFILE \
                && return 0
        [ -f $PIDFILE ] && return 1
        return 3
}

case "$1" in
  start)
        [ "$VERBOSE" != no ] && log_daemon_msg "Starting $DESC" "$NAME"
        do_start
        RETVAL=$?
        [ "$VERBOSE" != no ] &&
        case "$RETVAL" in
                0|1) log_end_msg 0 ;;
                *)   log_end_msg 1 ;;
        esac
        exit "$RETVAL"
        ;;
  stop)
        [ "$VERBOSE" != no ] && log_daemon_msg "Stopping $DESC" "$NAME"
        do_stop
        RETVAL=$?
        [ "$VERBOSE" != no ] &&
        case "$RETVAL" in
                0|1) log_end_msg 0 ;;
                *)   log_end_msg 1 ;;
        esac
        exit "$RETVAL"
        ;;
  #reload|force-reload)
        #
        # If do_reload() is not implemented then leave this commented out
        # and leave 'force-reload' as an alias for 'restart'.
        #
        #[ "$VERBOSE" != no ] && log_daemon_msg "Reloading $DESC" "$NAME"
        #do_reload
        #RETVAL=$?
        #[ "$VERBOSE" != no ] && log_end_msg $?
        #exit "$RETVAL"
        #;;
  restart|force-reload)
        #
        # If the "reload" option is implemented then remove the
        # 'force-reload' alias
        #
        [ "$VERBOSE" != no ] && log_daemon_msg "Restarting $DESC" "$NAME"
        do_restart
        RETVAL=$?
        [ "$VERBOSE" != no ] && log_end_msg "$RETVAL"
        exit "$RETVAL"
        ;;
  status)
    do_status
        RETVAL=$?
    [ "$VERBOSE" != no ] &&
        case "$RETVAL" in
          0) log_success_msg "$NAME is running" ;;
          *) log_failure_msg "$NAME is not running" ;;
        esac
    exit "$RETVAL"
        ;;
  *)
        echo "Usage: $SCRIPTNAME {start|stop|restart|force-reload|status}" >&2
        exit 3
        ;;
esac

:

# This was based off /etc/init.d/skeleton from the Ubuntu 8.04 Hardy release.
# (md5sum: da0162012b6a916bdbd4e2580282af78).  If we notice that changes, we
# should re-examine things.

# The configuration at the very top seems to be documented as part of the
# Linux Standard Base (LSB) Specification.  See section 20.6 Facility Names
# in particular.  This is also where I got the spec for the status parm.

# References:
# http://refspecs.linux-foundation.org/LSB_3.2.0/LSB-Core-generic/LSB-Core-generic.pdf
# Debian Policy SysV init: http://www.debian.org/doc/debian-policy/ch-opersys.html#s-sysvinit
# Examine files in /usr/share/doc/sysv-rc/
