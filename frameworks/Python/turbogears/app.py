from functools import partial
from operator import attrgetter
import os
from random import randint
import sys
import json

from jinja2 import Environment, PackageLoader
from sqlalchemy import create_engine
from sqlalchemy.orm import scoped_session, sessionmaker
from tg import expose, TGController, AppConfig

from models.Fortune import Fortune
from models.World import World


DBDRIVER = 'mysql'
DBHOSTNAME = 'tfb-database'
DATABASE_URI = '%s://benchmarkdbuser:benchmarkdbpass@%s:3306/hello_world?charset=utf8' % (DBDRIVER, DBHOSTNAME)

db_engine = create_engine(DATABASE_URI)
Session = sessionmaker(bind=db_engine)
db_session = Session()

env = Environment(loader=PackageLoader("app", "templates"), autoescape=True, auto_reload=False)

def getQueryNum(queryString):
    try:
        num_queries = int(queryString)
        if num_queries < 1:
            return 1
        if num_queries > 500:
            return 500
        return num_queries
    except ValueError:
         return 1

class RootController(TGController):

    @expose(content_type="text/plain")
    def plaintext(self):
        return "Hello, World!"

    @expose("json")
    def json(self):
        return {"message": "Hello, World!"}

    @expose("json")
    def db(self):
        wid = randint(1, 10000)
        world = db_session.query(World).get(wid).serialize()
        return world

    @expose("json")
    def updates(self, queries=1):
        num_queries = getQueryNum(queries)
        worlds = []
        rp = partial(randint, 1, 10000)
        ids = [rp() for _ in xrange(num_queries)]
        ids.sort()
        for id in ids:
            world = db_session.query(World).get(id)
            world.randomNumber = rp()
            worlds.append(world.serialize())
        db_session.commit()
        return json.dumps(worlds)

    @expose("json")
    def queries(self, queries=1):
        num_queries = getQueryNum(queries)
        rp = partial(randint, 1, 10000)
        get = db_session.query(World).get
        worlds = [get(rp()).serialize() for _ in xrange(num_queries)]
        return json.dumps(worlds)

    @expose()
    def fortunes(self):
        fortunes = db_session.query(Fortune).all()
        fortunes.append(Fortune(id=0, message="Additional fortune added at request time."))
        fortunes.sort(key=attrgetter("message"))
        template = env.get_template("fortunes.html")
        return template.render(fortunes=fortunes)

config = AppConfig(minimal=True, root_controller=RootController())
config.renderers.append("jinja")

tg_app = config.make_wsgi_app()


def app(env, start):
    try:
        return tg_app(env, start)
    finally:
        db_session.close()
