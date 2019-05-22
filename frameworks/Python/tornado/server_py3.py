#!/usr/bin/env python
import asyncio
import ujson as json
import motor
import uvloop
import tornado.ioloop
import tornado.web
import tornado.httpserver

from random import randint
from tornado.options import options
from commons import JsonHandler, JsonHelloWorldHandler, PlaintextHelloWorldHandler, HtmlHandler


options.define('port', default=8888, type=int, help="Server port")
options.define('mongo', default='localhost', type=str, help="MongoDB host")
options.define('backlog', default=8192, type=int, help="Server backlog")


class SingleQueryHandler(JsonHandler):

    async def get(self):
        world = await db.world.find_one(randint(1, 10000))
        world = {self.ID: int(world['_id']),
                 self.RANDOM_NUMBER: int(world[self.RANDOM_NUMBER])
                 }

        response = json.dumps(world)
        self.finish(response)


class MultipleQueriesHandler(JsonHandler):
    async def get(self):
        try:
            queries = int(self.get_argument(self.QUERIES))
        except Exception:
            queries = 1

        if queries < 1:
            queries = 1
        elif queries > 500:
            queries = 500

        worlds = []
        futures, _ = await asyncio.wait([db.world.find_one(randint(1, 10000)) for _ in range(queries)])

        for future in futures:
            world = future.result()

            worlds.append({self.ID: int(world['_id']),
                    self.RANDOM_NUMBER: int(world[self.RANDOM_NUMBER])})

        self.finish(json.dumps(worlds))


class UpdateHandler(JsonHandler):
    async def get(self):
        try:
            queries = int(self.get_argument(self.QUERIES))
        except Exception:
            queries = 1

        if queries < 1:
            queries = 1
        elif queries > 500:
            queries = 500

        worlds = []
        updates = []
        futures, _ = await asyncio.wait([db.world.find_one(randint(1, 10000)) for _ in range(queries)])

        for future in futures:
            world = future.result()
            new_value = randint(1, 10000)

            updates.append(db.world.update_one({'_id': world['_id']}, {"$set": {self.RANDOM_NUMBER: new_value}}))
            worlds.append({self.ID: int(world['_id']),
                    self.RANDOM_NUMBER: world[self.RANDOM_NUMBER]})
        await asyncio.wait(updates)

        self.finish(json.dumps(worlds))


class FortuneHandler(HtmlHandler):
    async def get(self):
        fortunes = [fortune async for fortune in db.fortune.find()]
        fortunes.append({'id': 0, 'message': 'Additional fortune added at request time.'})

        fortunes.sort(key=lambda f: f['message'])
        self.render('fortunes.html', fortunes=fortunes)


application = tornado.web.Application([
    (r"/json", JsonHelloWorldHandler),
    (r"/plaintext", PlaintextHelloWorldHandler),
    (r"/db", SingleQueryHandler),
    (r"/queries", MultipleQueriesHandler),
    (r"/updates", UpdateHandler),
    (r"/fortunes", FortuneHandler),
],
    template_path="templates"
)

application.ui_modules = {}

if __name__ == "__main__":
    uvloop.install()
    tornado.options.parse_command_line()
    server = tornado.httpserver.HTTPServer(application)
    server.bind(options.port, backlog=options.backlog, reuse_port=True)
    server.start(0)

    ioloop = tornado.ioloop.IOLoop.instance()
    db = motor.MotorClient(options.mongo, maxPoolSize=500).hello_world
    ioloop.start()
