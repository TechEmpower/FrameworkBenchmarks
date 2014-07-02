import tornado.ioloop
import tornado.web
import tornado.wsgi
import tornado.options
from tornado.options import options
import tornado.httpserver

tornado.options.define('port', default=8080, type=int, help="Server port")
tornado.options.define('procs', default=0, type=int, help="Number of processes (default: autodetect)")


import app
wsgiapp = tornado.wsgi.WSGIContainer(app.app.wsgi_app)

import logging
access_logger = logging.getLogger('tornado.access')
access_logger.propergate = False
access_logger.setLevel(logging.WARNING)


if __name__ == "__main__":
    tornado.options.parse_command_line()
    server = tornado.httpserver.HTTPServer(wsgiapp)
    server.bind(options.port)
    server.start(options.procs)
    tornado.ioloop.IOLoop.instance().start()
