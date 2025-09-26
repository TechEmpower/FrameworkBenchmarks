#!/usr/bin/env python
import bjoern
import falcon
import re
from email.utils import formatdate
from db_orm import session, World, Fortune
from helpers import load_template, FortuneTuple, generate_ids, sanitize
from operator import attrgetter
from random import randint


# setup
wsgi = app = falcon.App()

# Note:
# Bjoern doesn't provide any additional response headers like Date and Server
# so we need to provide them manually.
bjoern_version = [i for i in open('requirements-bjoern.txt', 'r') if re.search('bjoern', i)][0].strip().split('==')
server_info = '{}/{}'.format(*bjoern_version).title()


# resource endpoints
class JSONResource(object):
    def on_get(self, request, response):
        response.set_header('Date', formatdate(timeval=None, localtime=False, usegmt=True))
        response.set_header('Server', server_info)
        response.media = {'message': "Hello, world!"}


class RandomWorld(object):
    @session(serializable=False)
    def on_get(self, request, response):
        wid = randint(1, 10000)
        world = World[wid]
        response.set_header('Date', formatdate(timeval=None, localtime=False, usegmt=True))
        response.set_header('Server', server_info)
        response.media = world.to_dict()


class RandomQueries(object):
    @session(serializable=False)
    def on_get(self, request, response, **params):
        num = params.get("num", "1")
        num = sanitize(num)
        worlds = [World[ident].to_dict() for ident in generate_ids(num)]
        response.set_header('Date', formatdate(timeval=None, localtime=False, usegmt=True))
        response.set_header('Server', server_info)
        response.media = worlds


class UpdateQueries(object):
    @session(serializable=False)
    def on_get(self, request, response, **params):
        num = params.get("num", "1")
        num = sanitize(num)
        ids = generate_ids(num)
        ids.sort()
        worlds = []
        for item in ids:
            world = World[item]
            world.randomNumber = randint(1, 10000)
            worlds.append({"id": world.id, "randomNumber": world.randomNumber})
        response.set_header('Date', formatdate(timeval=None, localtime=False, usegmt=True))
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
        response.set_header('Date', formatdate(timeval=None, localtime=False, usegmt=True))
        response.set_header('Server', server_info)
        response.content_type = falcon.MEDIA_HTML
        response.text = content


class PlaintextResource(object):
    def on_get(self, request, response):
        response.set_header('Date', formatdate(timeval=None, localtime=False, usegmt=True))
        response.set_header('Server', server_info)
        response.content_type = falcon.MEDIA_TEXT
        response.text = 'Hello, world!'


# register resources
app.add_route("/json", JSONResource())
app.add_route("/db", RandomWorld())
app.add_route("/queries/{num}", RandomQueries())
app.add_route("/updates/{num}", UpdateQueries())
app.add_route("/fortunes", Fortunes())
app.add_route("/plaintext", PlaintextResource())


if __name__ == "__main__":
    host = '0.0.0.0'
    port = 8080

    bjoern.run(wsgi, host=host, port=port)
