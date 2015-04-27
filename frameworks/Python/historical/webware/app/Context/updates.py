import json
from random import randint
from functools import partial

from WebKit.HTTPContent import HTTPContent
from DbSession import Database
from World import World
import UrlHelper 

class updates(HTTPContent):
    def defaultAction(self):
        self.response().clearHeaders()
        self.response()._headers["Content-Type"] = "application/json"
        num_queries = UrlHelper.getQueryNum(self.request().field("queries"))
        worlds = []
        rp = partial(randint, 1, 10000)
        ids = [rp() for _ in xrange(num_queries)]
        ids.sort()
        for id in ids:
            world = Database.DbSession.query(World).get(id)
            world.randomNumber = rp()
            worlds.append(world.serialize())
        Database.DbSession.commit()
        output = json.dumps(worlds)
        self.response()._headers["Content-Length"] = len(output)
        self.write(output)
