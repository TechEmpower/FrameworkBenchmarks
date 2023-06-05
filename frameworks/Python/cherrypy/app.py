import os
import sys
from functools import partial
from operator import attrgetter
from random import randint
import json

import cherrypy
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy import Column
from sqlalchemy.types import String, Integer

try:
    from cgi import escape as html_escape
except:
    from html import escape as html_escape


Base = declarative_base()

if sys.version_info[0] == 3:
    xrange = range


def getQueryNum(queryString):
    try:
        int(queryString)
        return int(queryString)
    except ValueError:
        return 1


class Fortune(Base):
    __tablename__ = "fortune"

    id = Column(Integer, primary_key=True)
    message = Column(String)

    def serialize(self):
        return {'id': self.id, 'message': self.message}


class World(Base):
    __tablename__ = "world"

    id = Column(Integer, primary_key=True)
    randomNumber = Column(Integer)

    def serialize(self):
        return {'id': self.id, 'randomNumber': self.randomNumber}


class CherryPyBenchmark(object):
    @cherrypy.expose
    @cherrypy.tools.json_out()
    def json(self):
        cherrypy.response.headers["Content-Type"] = "application/json"
        json_message = {"message": "Hello, world!"}
        return json_message

    @cherrypy.expose
    def plaintext(self):
        cherrypy.response.headers["Content-Type"] = "text/plain"
        return "Hello, world!"

    @cherrypy.expose
    @cherrypy.tools.json_out()
    def db(self):
        cherrypy.response.headers["Content-Type"] = "application/json"
        wid = randint(1, 10000)
        world = cherrypy.request.db.query(World).get(wid).serialize()
        return world

    @cherrypy.expose
    @cherrypy.tools.json_out()
    def queries(self, queries=1):
        num_queries = getQueryNum(queries)
        if num_queries < 1:
            num_queries = 1
        if num_queries > 500:
            num_queries = 500

        rp = partial(randint, 1, 10000)
        get = cherrypy.request.db.query(World).get
        worlds = [get(rp()).serialize() for _ in xrange(num_queries)]
        return worlds

    @cherrypy.expose
    @cherrypy.tools.json_out()
    def updates(self, queries=1):
        cherrypy.response.headers["Content-Type"] = "application/json"
        num_queries = getQueryNum(queries)
        if num_queries < 1:
            num_queries = 1
        if num_queries > 500:
            num_queries = 500

        worlds = []
        rp = partial(randint, 1, 10000)
        ids = [rp() for _ in xrange(num_queries)]
        ids.sort()  # To avoid deadlock
        for id in ids:
            world = cherrypy.request.db.query(World).get(id)
            world.randomNumber = rp()
            worlds.append(world.serialize())
        cherrypy.request.db.commit()
        return worlds

    @cherrypy.expose
    def fortunes(self):
        _fortunes = cherrypy.request.db.query(Fortune).all()
        _fortunes.append( Fortune(id=0, message="Additional fortune added at request time.") )
        _fortunes.sort(key=attrgetter("message"))
        html = "<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>"
        for f in _fortunes:
            html += "<tr><td>" + str(f.id) + "</td><td>" + html_escape(f.message) + "</td></tr>"
        html += "</table></body></html>"
        return html


if __name__ == "__main__":
    import logging
    import multiprocessing
    # Register the SQLAlchemy plugin
    from saplugin import SAEnginePlugin
    DBDRIVER = 'mysql'
    DBUSER = 'benchmarkdbuser'
    DBPSWD = 'benchmarkdbpass'
    DATABASE_URI = '%s://%s:%s@tfb-database:3306/hello_world?charset=utf8' % (
        DBDRIVER, DBUSER, DBPSWD)
    SAEnginePlugin(cherrypy.engine, DATABASE_URI).subscribe()

    _is_travis = os.environ.get('TRAVIS') == 'true'

    workers = int(multiprocessing.cpu_count())
    if _is_travis:
        workers = 3

    cherrypy._cpconfig.environments['staging']['log.screen'] = False
    logging.getLogger("cherrypy").propagate = False
    log_fmt = "%(asctime)s.%(msecs)03d [%(levelname)s] (%(name)s) %(message)s"
    logging.basicConfig(level=logging.CRITICAL, format=log_fmt, datefmt="%Y-%m-%d %H:%M:%S")
    cherrypy.config.update({
            'tools.db.on': True,
            'environment': 'production',
            'log.screen': False,
            'log.access_file': '',
            'log.error_file': ''
    })    

    # Register the SQLAlchemy tool
    from satool import SATool
    cherrypy.tools.db = SATool()
    cherrypy.server.socket_host = '0.0.0.0'
    cherrypy.server.socket_port = 8080
    cherrypy.server.thread_pool = workers
    cherrypy.quickstart(CherryPyBenchmark(), '', {
        '/': {
            'tools.db.on': True,
            'log.screen': False,
            'log.access_file': '',
            'log.error_file': ''
        }
    })
