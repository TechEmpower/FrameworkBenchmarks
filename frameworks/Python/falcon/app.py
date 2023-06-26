#!/usr/bin/env python
import os
import sys
import falcon

from db_orm import session, World, Fortune
from helpers import load_template, FortuneTuple, generate_ids, sanitize
from operator import attrgetter
from random import randint
from email.utils import formatdate


# ------------------------------------------------------------------
# setup
wsgi = app = falcon.App()

response_server = "Falcon"
response_add_date = False

def add_ext_headers(response):
    if response_server:
        response.set_header('Server', response_server)

    if response_add_date:
        response.set_header('Date', formatdate(timeval=None, localtime=False, usegmt=True))


if os.getenv('USE_ORJSON', "0") == "1":
    import orjson
    # custom JSON handler
    JSONHandler = falcon.media.JSONHandler(dumps=orjson.dumps, loads=orjson.loads)
    extra_handlers = {
        "application/json": JSONHandler,
        "application/json; charset=UTF-8": JSONHandler
    }
    wsgi.req_options.media_handlers.update(extra_handlers)
    wsgi.resp_options.media_handlers.update(extra_handlers) 

# ------------------------------------------------------------------
# resource endpoints

class JSONResource(object):
    def on_get(self, request, response):
        add_ext_headers(response)
        response.media = {'message': "Hello, world!"}


class SingleQuery(object):
    @session(serializable=False)
    def on_get(self, request, response):
        wid = randint(1, 10000)
        world = World[wid]
        add_ext_headers(response)
        response.media = world.to_dict()


class MultipleQueries(object):
    @session(serializable=False)
    def on_get(self, request, response, num):
        num = sanitize(num)
        worlds = [World[ident].to_dict() for ident in generate_ids(num)]
        add_ext_headers(response)
        response.media = worlds


class UpdateQueries(object):
    @session(serializable=False)
    def on_get(self, request, response, num):
        num = sanitize(num)
        ids = generate_ids(num)
        ids.sort()
        worlds = []
        for item in ids:
            world = World[item]
            world.randomNumber = randint(1, 10000)
            worlds.append({"id": world.id, "randomNumber": world.randomNumber})
        add_ext_headers(response)
        response.media = worlds


class Fortunes(object):
    _template = load_template()

    @session(serializable=False)
    def on_get(self, request, response):
        fortunes = [FortuneTuple(id=f.id, message=f.message) for f in Fortune.select()]
        fortunes.append(FortuneTuple(id=0, message="Additional fortune added at request time."))
        fortunes.sort(key=attrgetter("message"))
        content = self._template.render(fortunes=fortunes)
        add_ext_headers(response)
        response.content_type = falcon.MEDIA_HTML
        response.text = content


class PlaintextResource(object):
    def on_get(self, request, response):
        add_ext_headers(response)
        response.content_type = falcon.MEDIA_TEXT
        response.text = 'Hello, world!'


# register resources
app.add_route("/json", JSONResource())
app.add_route("/db", SingleQuery())
app.add_route("/queries/{num}", MultipleQueries())
app.add_route("/updates/{num}", UpdateQueries())
app.add_route("/fortunes", Fortunes())
app.add_route("/plaintext", PlaintextResource())

# ------------------------------------------------------------------

if __name__ == "__main__":
    import optparse
    import multiprocessing
    import logging
    import re

    parser = optparse.OptionParser("usage: %prog [options]", add_help_option=False)
    parser.add_option("-h", "--host", dest="host", default='0.0.0.0', type="string")
    parser.add_option("-p", "--port", dest="port", default=8080, type="int")
    parser.add_option("-s", "--server", dest="server", default="gunicorn", type="string")
    parser.add_option("-w", "--workers", dest="workers", default=0, type="int")
    parser.add_option("-k", "--keepalive", dest="keepalive", default=60, type="int")
    parser.add_option("-v", "--verbose", dest="verbose", default=0, type="int")
    (opt, args) = parser.parse_args() 

    _is_travis = os.environ.get('TRAVIS') == 'true'

    workers = opt.workers
    if workers <= 0:
        workers = int(multiprocessing.cpu_count())

    if _is_travis:
        workers = 2

    if opt.server == 'waitress':
        import waitress
        response_server = None
        response_add_date = False
        logging.basicConfig()
        logging.getLogger().setLevel(logging.CRITICAL)
        logging.disable(True)
        if workers < 4:
            workers = 4
        waitress.serve(
            app=wsgi,
            listen=f"{opt.host}:{opt.port}",
            log_socket_errors=False,
            threads=workers,
            asyncore_use_poll=True,
            expose_tracebacks=False,
            connection_limit=128,
            channel_timeout=opt.keepalive,
            _quiet=True) 
        sys.exit(0)

    def run_app():
        global response_server
        global response_add_date
        
        if opt.server == 'bjoern':
            import bjoern
            # Note:
            # Bjoern doesn't provide any additional response headers like Date and Server
            # so we need to provide them manually.
            bjoern_version = [i for i in open('requirements-bjoern.txt', 'r') if re.search('bjoern', i)][0].strip().split('==')
            response_server = '{}/{}'.format(*bjoern_version).title()
            response_add_date = True
            bjoern.run(app, host=opt.host, port=opt.port, reuse_port=True)

        if opt.server == 'fastwsgi':
            import fastwsgi
            response_server = "FastWSGI"
            response_add_date = False
            fastwsgi.server.backlog = 4096
            fastwsgi.run(app, host=opt.host, port=opt.port, loglevel=opt.verbose)

        if opt.server == 'socketify':
            import socketify
            response_server = None
            response_add_date = False
            socketify.WSGI(app).listen(opt.port, lambda config: logging.info(f"Listening on port http://localhost:{opt.port} now\n")).run()

    def create_fork():
        n = os.fork()
        # n greater than 0 means parent process
        if not n > 0:
            run_app()

    # fork limiting the cpu count - 1
    for i in range(1, workers):
        create_fork()

    run_app()  # run app on the main process too :) 

