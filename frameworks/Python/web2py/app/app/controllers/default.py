# -*- coding: utf-8 -*-
from random import randint
from functools import partial
import json as jsonOut

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

def serializeWorld(world):
    return {
        "id" : world.id,
        "randomNumber" : world.randomNumber
    }

def serializeFortune(fortune):
    return {
        "id" : fortune.id,
        "message": fortune.message
    }

def plaintext():
    response.headers["Content-Type"]="text/plain; charset=UTF-8"
    return "Hello, World!"

def json():
    response.headers["Content-Type"]="application/json; charset=UTF-8"
    return jsonOut.dumps({"message":"Hello, World!"})

def db():
    response.headers["Content-Type"]="application/json; charset=UTF-8"
    wid = randint(1, 10000)
    world = DATABASE.world[wid]
    return jsonOut.dumps(serializeWorld(world))

def queries():
    response.headers["Content-Type"]="application/json; charset=UTF-8"
    num_queries = getQueryNum(request.vars["queries"])
    rp = partial(randint, 1, 10000)
    worlds = [serializeWorld(DATABASE.world[rp()]) for _ in xrange(num_queries)]
    return jsonOut.dumps(worlds)

def updates():
    response.headers["Content-Type"]="application/json; charset=UTF-8"
    num_queries = getQueryNum(request.vars["queries"])
    worlds = []
    rp = partial(randint, 1, 10000)
    ids = [rp() for _ in xrange(num_queries)]
    ids.sort() # To avoid deadlock
    for id in ids:
        world = DATABASE.world[id]
        newNumber = rp()
        DATABASE(DATABASE.world.id==id).update(randomNumber=newNumber)
        world.randomNumber = newNumber
        worlds.append(serializeWorld(world))
    return jsonOut.dumps(worlds)

def fortune():
    fortunes = DATABASE(DATABASE.fortune).select()
    fortune_list = fortunes.as_list();
    fortune_list.append({"id":0, "message":"Additional fortune added at request time."})
    fortune_list = sorted(fortune_list, key=lambda k: k["message"])
    return dict(fortunes=fortune_list)
