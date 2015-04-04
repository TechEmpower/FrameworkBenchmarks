# -*- coding: utf-8 -*-

# when web2py is run as a windows service (web2py.py -W)
# it does not load the command line options but it
# expects to find configuration settings in a file called
#
#   web2py/options.py
#
# this file is an example for options.py

import socket
import os

ip = '0.0.0.0'
port = 80
interfaces = [('0.0.0.0', 80)]
               #,('0.0.0.0',443,'ssl_private_key.pem','ssl_certificate.pem')]
password = '<recycle>'  # ## <recycle> means use the previous password
pid_filename = 'httpserver.pid'
log_filename = 'httpserver.log'
profiler_filename = None
ssl_certificate = ''  # 'ssl_certificate.pem'  # ## path to certificate file
ssl_private_key = ''  # 'ssl_private_key.pem'  # ## path to private key file
#numthreads = 50 # ## deprecated; remove
minthreads = None
maxthreads = None
server_name = socket.gethostname()
request_queue_size = 5
timeout = 30
shutdown_timeout = 5
folder = os.getcwd()
extcron = None
nocron = None
