import tornado.options
import tornado.wsgi
import tornado.httpserver
from tornado.options import options

tornado.options.define('port', default=8080, type=int, help=("Server port"))
tornado.options.parse_command_line()

import app
container = tornado.wsgi.WSGIContainer(app.app)
server = tornado.httpserver.HTTPServer(container)
server.bind(options.port)
server.start(8)
tornado.ioloop.IOLoop.instance().start()
