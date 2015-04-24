
from WebKit.HTTPContent import HTTPContent
from DbSession import Database
from World import World
import simplejson
from random import randint

class db(HTTPContent):
	def defaultAction(self):
		self.response().setHeader("Content-Type", "application/json; charset=UTF-8")
		wid = randint(1, 10000)
		world = Database.DbSession.query(World).get(wid).serialize()
   		self.write(simplejson.dumps(world))