#!/bin/bash
# the script should be run
# from WEB2PY root directory

prog="web2py.py"

chmod +x $prog

function web2py_start {
  nohup python2 ./$prog -a "<recycle>" >>/dev/null 2>/dev/null &
  pid=`pgrep -f $prog | tail -1`
  if [ "x$pid" != "x$$" ]
  then
      echo "WEB2PY has been started (pid $pid). Stop it with '$0 stop'"
  else
    echo "Failed to start WEB2PY."
  fi
}

function web2py_stop {
  pid="`pgrep -f $prog | grep -v $$`"
  if [ "x$pid" == "x" ]
    then
    echo "No WEB2PY processes to stop."
  else
    kill -15 $pid
    # Wait for web2py to shut down gracefully.
    sleep 2
    pid="`pgrep -f $prog | head -1`"
    if [ "x$pid" == "x" ]
    then
      echo "WEB2PY has been stopped."
    else
      echo "Failed to stop WEB2PY. (Possibly, only one of several web2py processes was killed.)"
      echo "Still running:"
      pgrep -af $prog
    fi
  fi
}

case "$1" in
  start)
    web2py_start
  ;;
  stop)
    web2py_stop
  ;;
  restart)
    web2py_stop
    web2py_start
  ;;
  *)
    echo "Usage: $0 [start|stop|restart]"
  ;;
esac

exit 0
