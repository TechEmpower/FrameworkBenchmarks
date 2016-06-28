# -*- coding: utf-8 -*-

#########################################################################
## This scaffolding model makes your app work on Google App Engine too
## File is released under public domain and you can use without limitations
#########################################################################

## if SSL/HTTPS is properly configured and you want all HTTP requests to
## be redirected to HTTPS, uncomment the line below:
# request.requires_https()

## app configuration made easy. Look inside private/appconfig.ini
from gluon.contrib.appconfig import AppConfig
## once in production, remove reload=True to gain full speed
myconf = AppConfig(reload=True)

DATABASE = None
DATABASE_URI = "mysql://benchmarkdbuser:benchmarkdbpass@127.0.0.1:3306/hello_world"

db = DAL(DATABASE_URI, fake_migrate_all=True)
DATABASE = db
## store sessions and tickets there
##session.connect(request, response, db=db)
## or store session in Memcache, Redis, etc.
## from gluon.contrib.memdb import MEMDB
## from google.appengine.api.memcache import Client
## session.connect(request, response, db = MEMDB(Client()))

## by default give a view/generic.extension to all actions from localhost
## none otherwise. a pattern can be 'controller/function.extension'
response.generic_patterns = [] 

db.define_table("world",
    Field("id"),
    Field("randomNumber")
)

db.define_table("fortune",
    Field("id"),
    Field("message")
)
