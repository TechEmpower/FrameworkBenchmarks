import tornado.ioloop
import tornado.web
from tornado import gen
import motor
import random
from tornado import escape
import tornado.options
from tornado.options import options


tornado.options.define('port', default=8888, type=int, help=(
    "Server port"))


db = motor.MotorClient("127.0.0.1").open_sync().hello_world

class JsonSerializeTestHandler(tornado.web.RequestHandler):
    @tornado.web.asynchronous
    @gen.coroutine
    def get(self):
        obj = dict(message="Hello, World!")
        self.write(obj)

class QueryTestHandler(tornado.web.RequestHandler):
    @tornado.web.asynchronous
    @gen.coroutine
    def get(self):
        queries = int(self.get_argument("queries", 0))

        if queries == 0:
            random_id = random.randint(1, 10000)
            world = yield gen.Task(db.world.find_one,{"randomNumber": random_id}, fields={"_id": 0, "id": 1, "randomNumber": 1})
            # Get first postion on arguments, and so first postion in mongo return
            world = world[0][0]
        else:
            worlds = []
            for i in xrange(int(queries)):
                random_id = random.randint(1, 10000)
                world = yield gen.Task(db.world.find_one,{"randomNumber": random_id}, fields={"_id": 0, "id": 1, "randomNumber": 1})
                # Get first postion on arguments, and so first postion in mongo return
                worlds.append(world[0][0])

            worlds = escape.json_encode(worlds)
            self.set_header("Content-Type", "application/json; charset=UTF-8")

        self.write(worlds if queries > 0 else world)

application = tornado.web.Application([
    (r"/json", JsonSerializeTestHandler),
    (r"/db", QueryTestHandler),
])

if __name__ == "__main__":
    tornado.options.parse_command_line()
    application.listen(options.port)
    tornado.ioloop.IOLoop.instance().start()
