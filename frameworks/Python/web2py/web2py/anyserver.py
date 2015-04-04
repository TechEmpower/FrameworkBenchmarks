#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
This file is part of the web2py Web Framework
Copyrighted by Massimo Di Pierro <mdipierro@cs.depaul.edu>
License: LGPLv3 (http://www.gnu.org/licenses/lgpl.html)

This file is based, although a rewrite, on MIT-licensed code from the Bottle web framework.
"""

import os
import sys
import optparse
import urllib

path = os.path.dirname(os.path.abspath(__file__))
os.chdir(path)
sys.path = [path] + [p for p in sys.path if not p == path]


class Servers:
    @staticmethod
    def cgi(app, address=None, **options):
        from wsgiref.handlers import CGIHandler
        CGIHandler().run(app)  # Just ignore host and port here

    @staticmethod
    def flup(app, address, **options):
        import flup.server.fcgi
        flup.server.fcgi.WSGIServer(app, bindAddress=address).run()

    @staticmethod
    def wsgiref(app, address, **options):  # pragma: no cover
        from wsgiref.simple_server import make_server, WSGIRequestHandler
        options = {}
        class QuietHandler(WSGIRequestHandler):
            def log_request(*args, **kw):
                pass
        options['handler_class'] = QuietHandler
        srv = make_server(address[0], address[1], app, **options)
        srv.serve_forever()

    @staticmethod
    def cherrypy(app, address, **options):
        from cherrypy import wsgiserver
        server = wsgiserver.CherryPyWSGIServer(address, app)
        server.start()

    @staticmethod
    def rocket(app, address, **options):
        from gluon.rocket import CherryPyWSGIServer
        server = CherryPyWSGIServer(address, app)
        server.start()

    @staticmethod
    def rocket_with_repoze_profiler(app, address, **options):
        from gluon.rocket import CherryPyWSGIServer
        from repoze.profile.profiler import AccumulatingProfileMiddleware
        from gluon.settings import global_settings
        global_settings.web2py_crontype = 'none'
        wrapped = AccumulatingProfileMiddleware(
            app,
            log_filename='wsgi.prof',
            discard_first_request=True,
            flush_at_shutdown=True,
            path='/__profile__'
        )
        server = CherryPyWSGIServer(address, wrapped)
        server.start()

    @staticmethod
    def paste(app, address, **options):
        options = {}
        from paste import httpserver
        from paste.translogger import TransLogger
        httpserver.serve(app, host=address[0], port=address[1], **options)

    @staticmethod
    def fapws(app, address, **options):
        import fapws._evwsgi as evwsgi
        from fapws import base
        evwsgi.start(address[0], str(address[1]))
        evwsgi.set_base_module(base)

        def app(environ, start_response):
            environ['wsgi.multiprocess'] = False
            return app(environ, start_response)
        evwsgi.wsgi_cb(('', app))
        evwsgi.run()

    @staticmethod
    def gevent(app, address, **options):
        options = options['options']
        workers = options.workers
        from gevent import pywsgi
        from gevent.pool import Pool
        pywsgi.WSGIServer(address, app, spawn=workers and Pool(
            int(options.workers)) or 'default', log=None).serve_forever()

    @staticmethod
    def bjoern(app, address, **options):
        import bjoern
        bjoern.run(app, *address)

    @staticmethod
    def tornado(app, address, **options):
        import tornado.wsgi
        import tornado.httpserver
        import tornado.ioloop
        container = tornado.wsgi.WSGIContainer(app)
        server = tornado.httpserver.HTTPServer(container)
        server.listen(address=address[0], port=address[1])
        tornado.ioloop.IOLoop.instance().start()

    @staticmethod
    def twisted(app, address, **options):
        from twisted.web import server, wsgi
        from twisted.python.threadpool import ThreadPool
        from twisted.internet import reactor
        thread_pool = ThreadPool()
        thread_pool.start()
        reactor.addSystemEventTrigger('after', 'shutdown', thread_pool.stop)
        factory = server.Site(wsgi.WSGIResource(reactor, thread_pool, app))
        reactor.listenTCP(address[1], factory, interface=address[0])
        reactor.run()

    @staticmethod
    def diesel(app, address, **options):
        from diesel.protocols.wsgi import WSGIApplication
        app = WSGIApplication(app, port=address[1])
        app.run()

    @staticmethod
    def gunicorn(app, address, **options):
        options = {}
        from gunicorn.app.base import Application
        config = {'bind': "%s:%d" % address}
        config.update(options)
        sys.argv = ['anyserver.py']

        class GunicornApplication(Application):
            def init(self, parser, opts, args):
                return config

            def load(self):
                return app
        g = GunicornApplication()
        g.run()

    @staticmethod
    def eventlet(app, address, **options):
        from eventlet import wsgi, listen
        wsgi.server(listen(address), app)

    @staticmethod
    def mongrel2(app, address, **options):
        import uuid
        sys.path.append(os.path.abspath(os.path.dirname(__file__)))
        from mongrel2 import handler
        conn = handler.Connection(str(uuid.uuid4()),
                                  "tcp://127.0.0.1:9997",
                                  "tcp://127.0.0.1:9996")
        mongrel2_handler(app, conn, debug=False)

    @staticmethod
    def motor(app, address, **options):
        #https://github.com/rpedroso/motor
        import motor
        app = motor.WSGIContainer(app)
        http_server = motor.HTTPServer(app)
        http_server.listen(address=address[0], port=address[1])
        #http_server.start(2)
        motor.IOLoop.instance().start()

    @staticmethod
    def pulsar(app, address, **options):
        from pulsar.apps import wsgi
        sys.argv = ['anyserver.py']
        s = wsgi.WSGIServer(callable=app, bind="%s:%d" % address)
        s.start()


def mongrel2_handler(application, conn, debug=False):
    """
    Based on :
    https://github.com/berry/Mongrel2-WSGI-Handler/blob/master/wsgi-handler.py

    WSGI handler based on the Python wsgiref SimpleHandler.
    A WSGI application should return a iterable op StringTypes.
    Any encoding must be handled by the WSGI application itself.
    """
    from wsgiref.handlers import SimpleHandler
    try:
        import cStringIO as StringIO
    except:
        import StringIO

    # TODO - this wsgi handler executes the application and renders a page
    # in memory completely before returning it as a response to the client.
    # Thus, it does not "stream" the result back to the client. It should be
    # possible though. The SimpleHandler accepts file-like stream objects. So,
    # it should be just a matter of connecting 0MQ requests/response streams to
    # the SimpleHandler requests and response streams. However, the Python API
    # for Mongrel2 doesn't seem to support file-like stream objects for requests
    # and responses. Unless I have missed something.

    while True:
        if debug:
            print "WAITING FOR REQUEST"

        # receive a request
        req = conn.recv()
        if debug:
            print "REQUEST BODY: %r\n" % req.body

        if req.is_disconnect():
            if debug:
                print "DISCONNECT"
            continue  # effectively ignore the disconnect from the client

        # Set a couple of environment attributes a.k.a. header attributes
        # that are a must according to PEP 333
        environ = req.headers
        environ['SERVER_PROTOCOL'] = 'HTTP/1.1'  # SimpleHandler expects a server_protocol, lets assume it is HTTP 1.1
        environ['REQUEST_METHOD'] = environ['METHOD']
        if ':' in environ['Host']:
            environ['SERVER_NAME'] = environ['Host'].split(':')[0]
            environ['SERVER_PORT'] = environ['Host'].split(':')[1]
        else:
            environ['SERVER_NAME'] = environ['Host']
            environ['SERVER_PORT'] = ''
        environ['SCRIPT_NAME'] = ''  # empty for now
        environ['PATH_INFO'] = urllib.unquote(environ['PATH'])
        if '?' in environ['URI']:
            environ['QUERY_STRING'] = environ['URI'].split('?')[1]
        else:
            environ['QUERY_STRING'] = ''
        if 'Content-Length' in environ:
            environ['CONTENT_LENGTH'] = environ[
                'Content-Length']  # necessary for POST to work with Django
        environ['wsgi.input'] = req.body

        if debug:
            print "ENVIRON: %r\n" % environ

        # SimpleHandler needs file-like stream objects for
        # requests, errors and responses
        reqIO = StringIO.StringIO(req.body)
        errIO = StringIO.StringIO()
        respIO = StringIO.StringIO()

        # execute the application
        handler = SimpleHandler(reqIO, respIO, errIO, environ,
                                multithread=False, multiprocess=False)
        handler.run(application)

        # Get the response and filter out the response (=data) itself,
        # the response headers,
        # the response status code and the response status description
        response = respIO.getvalue()
        response = response.split("\r\n")
        data = response[-1]
        headers = dict([r.split(": ") for r in response[1:-2]])
        code = response[0][9:12]
        status = response[0][13:]

        # strip BOM's from response data
        # Especially the WSGI handler from Django seems to generate them (2 actually, huh?)
        # a BOM isn't really necessary and cause HTML parsing errors in Chrome and Safari
        # See also: http://www.xs4all.nl/~mechiel/projects/bomstrip/
        # Although I still find this a ugly hack, it does work.
        data = data.replace('\xef\xbb\xbf', '')

        # Get the generated errors
        errors = errIO.getvalue()

        # return the response
        if debug:
            print "RESPONSE: %r\n" % response
        if errors:
            if debug:
                print "ERRORS: %r" % errors
            data = "%s\r\n\r\n%s" % (data, errors)
        conn.reply_http(
            req, data, code=code, status=status, headers=headers)


def run(servername, ip, port, softcron=True, logging=False, profiler=None,
        options=None):
    if servername == 'gevent':
        from gevent import monkey
        monkey.patch_all()
    elif servername == 'eventlet':
        import eventlet
        eventlet.monkey_patch()

    import gluon.main

    if logging:
        application = gluon.main.appfactory(wsgiapp=gluon.main.wsgibase,
                                            logfilename='httpserver.log',
                                            profiler_dir=profiler)
    else:
        application = gluon.main.wsgibase
    if softcron:
        from gluon.settings import global_settings
        global_settings.web2py_crontype = 'soft'
    getattr(Servers, servername)(application, (ip, int(port)), options=options)



def main():
    usage = "python anyserver.py -s tornado -i 127.0.0.1 -p 8000 -l -P"
    try:
        version = open('VERSION','r')
    except IOError:
        version = ''
    parser = optparse.OptionParser(usage, None, optparse.Option, version)
    parser.add_option('-l',
                      '--logging',
                      action='store_true',
                      default=False,
                      dest='logging',
                      help='log into httpserver.log')
    parser.add_option('-P',
                      '--profiler',
                      default=False,
                      dest='profiler_dir',
                      help='profiler dir')
    servers = ', '.join(x for x in dir(Servers) if not x[0] == '_')
    parser.add_option('-s',
                      '--server',
                      default='rocket',
                      dest='server',
                      help='server name (%s)' % servers)
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
                      help='number of workers number')
    (options, args) = parser.parse_args()
    print 'starting %s on %s:%s...' % (
        options.server, options.ip, options.port)
    run(options.server, options.ip, options.port,
        logging=options.logging, profiler=options.profiler_dir,
        options=options)

if __name__ == '__main__':
    main()
