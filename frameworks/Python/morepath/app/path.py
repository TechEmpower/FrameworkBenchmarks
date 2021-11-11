from random import randint

from .app import App
from .model import Json, World, WorldQueries, WorldUpdates, Plaintext
from .collection import FortuneCollection


@App.path(model=Json, path='json')
def get_json():
    return Json()


@App.path(model=World, path='db')
def get_random_world():
    return World[randint(1, 10000)]


@App.path(model=WorldQueries, path='queries')
def get_queries(queries):
    return WorldQueries(queries)


@App.path(model=FortuneCollection, path='fortunes')
def get_fortunes():
    return FortuneCollection()


@App.path(model=WorldUpdates, path='updates')
def get_updates(queries):
    return WorldUpdates(queries)


@App.path(model=Plaintext, path='plaintext')
def get_plaintext():
    return Plaintext()
