import json
from random import randint
from functools import partial

from WebKit.HTTPContent import HTTPContent
from DbSession import Database
from World import World
import UrlHelper 

class queries(HTTPContent):
    def defaultAction(self):
        self.response().clearHeaders()
        self.response()._headers["Content-Type"] = "application/json"
        num_queries = UrlHelper.getQueryNum(self.request().field("queries"))
        rp = partial(randint, 1, 10000)
        get = Database.DbSession.query(World).get
        worlds = [get(rp()).serialize() for _ in xrange(num_queries)]
        output = json.dumps(worlds)
        self.response()._headers["Content-Length"] = len(output)
        self.write(output)
