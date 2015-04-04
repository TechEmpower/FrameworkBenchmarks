#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys
import os
import optparse

if hasattr(sys, 'frozen'):
    path = os.path.dirname(os.path.abspath(sys.executable))
elif '__file__' in globals():
    path = os.path.dirname(os.path.abspath(__file__))
else:
    path = os.getcwd()
os.chdir(path)

sys.path = [path] + [p for p in sys.path if not p == path]

from gevent import pywsgi
from gevent.pool import Pool
from gevent import monkey
monkey.patch_all()

def run(options):
    import gluon.main
    if options.password != '<recycle>':
        gluon.main.save_password(options.password, int(options.port))
    if options.logging:
        application = gluon.main.appfactory(wsgiapp=gluon.main.wsgibase,
                                            logfilename='httpserver.log',
                                            profiler_dir=profiler)
    else:
        application = gluon.main.wsgibase
    address = (options.ip, int(options.port))
    workers = options.workers
    spawn = workers and Pool(int(options.workers)) or 'default'
    ssl_args = dict()
    if options.ssl_private_key:
        ssl_args['keyfile'] = options.ssl_private_key
    if options.ssl_certificate:
        ssl_args['certfile'] = options.ssl_certificate
    server = pywsgi.WSGIServer(
                    address, application,
                    spawn=spawn, log=None,
                    **ssl_args
                    )
    server.serve_forever()

def main():
    usage = 'python web2py_gevent.py -i 127.0.0.1 -p 8000 -a "<recycle>"'
    try:
        version = open('VERSION','r')
    except IOError:
        version = ''
    parser = optparse.OptionParser(usage, None, optparse.Option, version)
    msg = ('password to be used for administration '
           '(use -a "<recycle>" to reuse the last password))')
    parser.add_option('-a',
                      '--password',
                      default='<recycle>',
                      dest='password',
                      help=msg)

    parser.add_option('-c',
                      '--ssl_certificate',
                      default='',
                      dest='ssl_certificate',
                      help='file that contains ssl certificate')

    parser.add_option('-k',
                      '--ssl_private_key',
                      default='',
                      dest='ssl_private_key',
                      help='file that contains ssl private key')

    parser.add_option('-l',
                      '--logging',
                      action='store_true',
                      default=False,
                      dest='logging',
                      help='log into httpserver.log')

    parser.add_option('-F',
                      '--profiler',
                      dest='profiler_dir',
                      default=None,
                      help='profiler dir')

    parser.add_option('-i',
                      '--ip',
                      default='127.0.0.1',
                      dest='ip',
                      help='ip address')

    parser.add_option('-p',
                      '--port',
                      default='8000',
                      dest='port',
                      help='port number')

    parser.add_option('-w',
                      '--workers',
                      default=None,
                      dest='workers',
                      help='number of workers')

    (options, args) = parser.parse_args()
    print 'starting on %s:%s...' % (
        options.ip, options.port)
    run(options)

if __name__ == '__main__':
    main()
