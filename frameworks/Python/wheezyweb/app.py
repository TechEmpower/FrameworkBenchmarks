import os
import sys
from functools import partial
from operator import attrgetter
from random import randint

import bleach

from wheezy.http import HTTPResponse
from wheezy.http import WSGIApplication
from wheezy.routing import url
from wheezy.web.handlers import BaseHandler
from wheezy.web.middleware import bootstrap_defaults
from wheezy.web.middleware import path_routing_middleware_factory

from wheezy.template.engine import Engine
from wheezy.template.ext.core import CoreExtension
from wheezy.template.loader import FileLoader

from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy import create_engine, Column
from sqlalchemy.types import String, Integer, Unicode
from sqlalchemy.orm import sessionmaker

from meinheld import server

DBDRIVER = 'mysql'
DBHOSTNAME = os.environ.get('DBHOST', 'localhost')
DATABASE_URI = '%s://benchmarkdbuser:benchmarkdbpass@%s:3306/hello_world?charset=utf8' % (DBDRIVER, DBHOSTNAME)

Base = declarative_base()
db_engine = create_engine(DATABASE_URI)
Session = sessionmaker(bind=db_engine)
db_session = Session()

if sys.version_info[0] == 3:
    xrange = range

def getQueryNum(queryString):
    try:
        int(queryString)
        return int(queryString)
    except ValueError:
        return 1

class Fortune(Base):
    __tablename__ = "Fortune"
    id = Column(Integer, primary_key=True)
    message = Column(String)

    def serialize(self):
        return {
            'id': self.id,
            'randomNumber': self.randomNumber,
        }

class World(Base):
    __tablename__ = "World"
    id = Column(Integer, primary_key=True)
    randomNumber = Column(Integer)
    def serialize(self):
        return {
            'id': self.id,
            'randomNumber': self.randomNumber,
        }

class JsonHandler(BaseHandler):
    def get(self):
        response = self.json_response({"message": "Hello, World!"})
        response.headers = [("Content-Type", "application/json; charset=UTF-8")]
        return response

class DbHandler(BaseHandler):
    def get(self):
        db_engine.connect()
        wid = randint(1, 10000)
        world = db_session.query(World).get(wid).serialize()
        return self.json_response(world)

class QueriesHandler(BaseHandler):
    def get(self):
        queries = self.request.get_param("queries")
        num_queries = getQueryNum(queries)
        if num_queries < 1:
            num_queries = 1
        if num_queries > 500:
            num_queries = 500
        rp = partial(randint, 1, 10000)
        get = db_session.query(World).get
        worlds = [get(rp()).serialize() for _ in xrange(num_queries)]
        return self.json_response(worlds)

class UpdatesHandler(BaseHandler):
    def get(self):
        queries = self.request.get_param("queries")
        num_queries = getQueryNum(queries)
        if num_queries < 1:
            num_queries = 1
        if num_queries > 500:
            num_queries = 500
        worlds = []
        rp = partial(randint, 1, 10000)
        ids = [rp() for _ in xrange(num_queries)]
        ids.sort() # To avoid deadlock
        for id in ids:
            world = db_session.query(World).get(id)
            world.randomNumber = rp()
            worlds.append(world.serialize())
        db_session.commit()
        return self.json_response(worlds)

class FortuneHandler(BaseHandler):
    def get(self):
        fortunes = db_session.query(Fortune).all()
        fortunes.append(Fortune(id=0, message="Additional fortune added at request time."))
        fortunes.sort(key=attrgetter("message"))
        engine = Engine(loader=FileLoader(["views"]), extensions=[CoreExtension()])
        template = engine.get_template("fortune.html")
        for f in fortunes:
            f.message = bleach.clean(f.message)
        template_html = template.render({"fortunes": fortunes})		

        response = HTTPResponse()
        response.write(template_html)
        return response

def plaintext(request):
    response = HTTPResponse()
    response.headers = [("Content-Type", "text/plain; charset=UTF-8")]
    response.write("Hello, world!")
    return response

all_urls = [
    url("plaintext", plaintext, name="plaintext"),
    url("json", JsonHandler, name="json"),
    url("db", DbHandler, name="db"),
    url("queries", QueriesHandler, name="queries"),
    url("updates", UpdatesHandler, name="updates"),
    url("fortune", FortuneHandler, name="fortune")
]

options = {}

app = WSGIApplication(
    middleware = [
        bootstrap_defaults(url_mapping=all_urls),
        path_routing_middleware_factory
    ],
    options = options
)

if __name__ == '__main__':
    server.listen(("127.0.0.1", 8080))
    server.run(app)
