import cgi
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
    def fortune(self):
        fortunes = cherrypy.request.db.query(Fortune).all()
        fortunes.append(
            Fortune(id=0, message="Additional fortune added at request time."))
        fortunes.sort(key=attrgetter("message"))
        html = "<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>"
        for f in fortunes:
            html += "<tr><td>" + str(f.id) + "</td><td>" + cgi.escape(
                f.message) + "</td></tr>"
        html += "</table></body></html>"
        return html


if __name__ == "__main__":
    # Register the SQLAlchemy plugin
    from saplugin import SAEnginePlugin
    DBDRIVER = 'mysql'
    DATABASE_URI = '%s://benchmarkdbuser:benchmarkdbpass@tfb-database:3306/hello_world?charset=utf8' % (
        DBDRIVER)
    SAEnginePlugin(cherrypy.engine, DATABASE_URI).subscribe()

    # Register the SQLAlchemy tool
    from satool import SATool
    cherrypy.tools.db = SATool()
    cherrypy.server.socket_host = '0.0.0.0'
    cherrypy.quickstart(CherryPyBenchmark(), '', {
        '/': {
            'tools.db.on': True,
            'log.screen': False,
            'log.access_file': ''
        }
    })
