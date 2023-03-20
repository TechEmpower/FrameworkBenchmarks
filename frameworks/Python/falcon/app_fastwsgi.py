#!/usr/bin/env python
import fastwsgi
import falcon
import re
from db_orm import session, World, Fortune
from helpers import load_template, FortuneTuple, generate_ids, sanitize
from operator import attrgetter
from random import randint


# setup
app = falcon.App()

server_info = 'FastWSGI+Falcon'


# resource endpoints
class JSONResource(object):
    def on_get(self, request, response):
        response.set_header('Server', server_info)
        response.media = {'message': "Hello, world!"}


class SingleQuery(object):
    @session(serializable=False)
    def on_get(self, request, response):
        wid = randint(1, 10000)
        world = World[wid]
        response.set_header('Server', server_info)
        response.media = world.to_dict()


class MultipleQueries(object):
    @session(serializable=False)
    def on_get(self, request, response, num):
        num = sanitize(num)
        worlds = [World[ident].to_dict() for ident in generate_ids(num)]
        response.set_header('Server', server_info)
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
        response.set_header('Server', server_info)
        response.media = worlds


class Fortunes(object):
    _template = load_template()

    @session(serializable=False)
    def on_get(self, request, response):
        fortunes = [FortuneTuple(id=f.id, message=f.message) for f in Fortune.select()]
        fortunes.append(FortuneTuple(id=0, message="Additional fortune added at request time."))
        fortunes.sort(key=attrgetter("message"))
        content = self._template.render(fortunes=fortunes)
        response.set_header('Server', server_info)
        response.content_type = falcon.MEDIA_HTML
        response.text = content


class PlaintextResource(object):
    def on_get(self, request, response):
        response.set_header('Server', server_info)
        response.content_type = falcon.MEDIA_TEXT
        response.text = 'Hello, world!'


# register resources
app.add_route("/json", JSONResource())
app.add_route("/db", SingleQuery())
app.add_route("/queries/{num}", MultipleQueries())
app.add_route("/updates/{num}", UpdateQueries())
app.add_route("/fortunes", Fortunes())
app.add_route("/plaintext", PlaintextResource())


if __name__ == "__main__":
    import os
    import multiprocessing

    _is_travis = os.environ.get('TRAVIS') == 'true'

    workers = int(multiprocessing.cpu_count())
    if _is_travis:
        workers = 2

    host = '0.0.0.0'
    port = 8080

    def run_app():
        fastwsgi.run(app, host=host, port=port, loglevel=0)

    def create_fork():
        n = os.fork()
        # n greater than 0 means parent process
        if not n > 0:
            run_app()

    # fork limiting the cpu count - 1
    for i in range(1, workers):
        create_fork()

    run_app()  # run app on the main process too :)
