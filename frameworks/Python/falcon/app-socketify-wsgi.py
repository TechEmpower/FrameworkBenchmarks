#!/usr/bin/env python
import falcon
from socketify import WSGI
import os
import multiprocessing
import logging

# setup
wsgi = app = falcon.App()

# resource endpoints
class JSONResource(object):
    def on_get(self, request, response):
        response.media = {'message': "Hello, world!"}

class PlaintextResource(object):
    def on_get(self, request, response):
        response.content_type = falcon.MEDIA_TEXT
        response.text = 'Hello, world!'


# register resources
app.add_route("/json", JSONResource())
app.add_route("/plaintext", PlaintextResource())


_is_travis = os.environ.get('TRAVIS') == 'true'

workers = int(multiprocessing.cpu_count())
if _is_travis:
    workers = 2


def run_app():
    WSGI(app).listen(8080, lambda config: logging.info(f"Listening on port http://localhost:{config.port} now\n")).run()


def create_fork():
    n = os.fork()
    # n greater than 0 means parent process
    if not n > 0:
        run_app()


# fork limiting the cpu count - 1
for i in range(1, multiprocessing.cpu_count()):
    create_fork()

run_app()  # run app on the main process too :)
