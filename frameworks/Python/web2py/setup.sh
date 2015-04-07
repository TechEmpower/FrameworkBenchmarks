export PY2_ROOT=$IROOT/py2
export PY2=$PY2_ROOT/bin/python

sudo chmod a+w web2py/httpserver.pid & $PY2 web2py/web2py.py -a '' -i 127.0.0.1 -p 8080  &
