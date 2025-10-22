import os
import sys
import time
from email.utils import formatdate

try:
    from ujson import dumps as jsonify
except:
    from json import dumps as jsonify

g_time = 0
g_asctime = ""

def application(environ, start_response):
    global g_time
    global g_asctime
    
    path = environ["PATH_INFO"]
    headers = [ ('Server', 'uWSGI') ]

    curr_time = int(time.time())
    if curr_time != g_time:
        g_time = curr_time
        g_asctime = formatdate(timeval=None, localtime=False, usegmt=True)
        
    headers.append( ('Date', g_asctime ) )

    if path == '/plaintext':
        data = b'Hello, World!'
        headers.append( ('Content-Type', 'text/plain') )
        headers.append( ('Content-Length', str(len(data))) )
        start_response('200 OK', headers)
        return [ data ]
    
    if path == '/json':
        data = jsonify( {"message": "Hello, World!"} ).encode('utf8')
        headers.append( ('Content-Type', 'application/json') )
        headers.append( ('Content-Length', str(len(data))) )
        start_response('200 OK', headers)
        return [ data ]

    headers.append( ('Content-Length', '0') )
    start_response('400 Bad Request', headers)
    return [ b'' ]
