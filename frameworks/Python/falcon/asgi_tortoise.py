#!/usr/bin/env python
import falcon.asgi
from falcon import HTTPInternalServerError
from helpers import load_template, FortuneTuple, generate_ids, sanitize
from operator import attrgetter
from random import randint
from tortoise import Tortoise
from tortoise_models import World, Fortune
from tortoise.transactions import in_transaction
from tortoise.exceptions import OperationalError


# Middleware
class TortoiseInit:
    async def process_startup(self, scope, event):
        await Tortoise.init(
            db_url="postgres://benchmarkdbuser:benchmarkdbpass@tfb-database:5432/hello_world",
            modules={"models": ["tortoise_models"]}
        )
        await Tortoise.generate_schemas(safe=True)

    async def process_shutdown(self, scopre, event):
        await Tortoise.close_connections()


tortoise_init = TortoiseInit()


# resource endpoints
class JSONResource(object):
    async def on_get(self, request, response):
        response.media = {'message': "Hello, world!"}


class SingleQuery(object):
    async def on_get(self, request, response):
        resp = await World.get(id=randint(1, 10000))
        response.media = resp.to_dict()


class MultipleQueries(object):
    async def on_get(self, request, response, num):
        try:
            async with in_transaction():
                num = sanitize(num)
                items = await World.filter(id__in=generate_ids(num)).values()
                response.media = [i.to_dict() for i in items]
        except OperationalError:
            raise HTTPInternalServerError()


class UpdateQueries(object):
    async def on_get(self, request, response, num):
        try:
            async with in_transaction():
                num = sanitize(num)
                items = await World.filter(id__in=generate_ids(num))
                worlds = []
                for idx in items:
                    idx.randomNumber = randint(1, 10000)
                    await idx.save()
                    worlds.append({'id': idx.id, 'randomNumber': idx.randomNumber})
                response.media = worlds
        except OperationalError:
            raise HTTPInternalServerError()


class Fortunes(object):
    _template = load_template()

    async def on_get(self, request, response):
        fortunes = [FortuneTuple(id=f.id, message=f.message) for f in await Fortune.all()]
        fortunes.append(FortuneTuple(id=0, message="Additional fortune added at request time."))
        fortunes.sort(key=attrgetter("message"))
        content = self._template.render(fortunes=fortunes)
        response.content_type = falcon.MEDIA_HTML
        response.text = content


class PlaintextResource(object):
    async def on_get(self, request, response):
        response.content_type = falcon.MEDIA_TEXT
        response.text = 'Hello, world!'


asgi = falcon.asgi.App(middleware=[tortoise_init])
# register resources
asgi.add_route("/json", JSONResource())
asgi.add_route("/db", SingleQuery())
asgi.add_route("/queries/{num}", MultipleQueries())
asgi.add_route("/updates/{num}", UpdateQueries())
asgi.add_route("/fortunes", Fortunes())
asgi.add_route("/plaintext", PlaintextResource())
