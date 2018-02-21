# -*- coding: utf-8 -*-
from random import randint
from functools import partial
import json as jsonOut

from gluon import current
from database import Dal, RawDal, num_queries

def plaintext():
    current.response.headers['Content-Type'] = 'text/plain'
    return 'Hello, World!'

def json():
    current.response.headers['Content-Type'] = 'application/json'
    return jsonOut.dumps({'message': 'Hello, World!'})

def db():
    current.response.headers['Content-Type']='application/json'
    return jsonOut.dumps(Dal('World').get_world(randint(1, 10000)))

def queries():
    current.response.headers['Content-Type']='application/json'
    db = RawDal() if current.optimized else Dal('World')
    get_world = db.get_world
    r10k = partial(randint, 1, 10000)
    worlds = [get_world(r10k()) for _ in
              xrange(num_queries(current.request.vars.queries))]
    return jsonOut.dumps(worlds)

def updates():
    current.response.headers['Content-Type']='application/json'
    db = RawDal() if current.optimized else Dal('World')
    get_world = db.get_world
    update_world = db.update_world
    r10k = partial(randint, 1, 10000)
    worlds = []
    for wid in (r10k() for _ in xrange(num_queries(current.request.vars.queries))):
        world = get_world(wid)
        newNumber = r10k()
        world['randomNumber'] = newNumber
        worlds.append(world)
        update_world(wid, newNumber)
    return jsonOut.dumps(worlds)

def fortune():
    new_message = {'id': 0, 'message': 'Additional fortune added at request time.'}
    db = RawDal() if current.optimized else Dal('Fortune')
    fortunes = db.get_fortunes(new_message=new_message)
    return current.response.render('fortune.html', fortunes=fortunes)
