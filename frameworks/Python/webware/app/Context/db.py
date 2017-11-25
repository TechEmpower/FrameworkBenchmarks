
from WebKit.HTTPContent import HTTPContent
from DbSession import Database
from World import World
import json
from random import randint

class db(HTTPContent):
    def defaultAction(self):
        self.response().clearHeaders()
        self.response()._headers["Content-Type"] = "application/json"
        wid = randint(1, 10000)
        world = Database.DbSession.query(World).get(wid).serialize()
        output = json.dumps(world)
        self.response()._headers["Content-Length"] = len(output)
        self.write(output)
