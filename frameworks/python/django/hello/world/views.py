# Create your views here.

from django.template import Context, loader
from django.http import HttpResponse
from django.core import serializers
from world.models import World, Fortune
from django.shortcuts import render
from ujson import dumps as uj_dumps
import random
import sys
from operator import attrgetter
from functools import partial

if sys.version_info[0] == 3:
  xrange = range

def _get_queries(request):
  try:
    queries = int(request.GET.get('queries', 1))
  except Exception:
    queries = 1
  if queries < 1:
    queries = 1
  if queries > 500:
    queries = 500
  return queries


def json(request):
  response = {
    "message": "Hello, World!"
  }
  return HttpResponse(uj_dumps(response), mimetype="application/json")

def db(request):
  r = random.randint(1, 10000)
  world = uj_dumps({'id' : r, 'randomNumber' : World.objects.get(id=r).randomnumber})
  return HttpResponse(world, mimetype="application/json")

def dbs(request):
  queries = _get_queries(request)

  # fun fact:  every dot-notation lookup calls some python magic under the hood.  Like every other code,
  # one can eliminate dereferences by storing the end dereferenced thing in an identifier
  g = World.objects.get

  # but wait!  there's more!  if we're calling a function over and over with the same parameters, 
  # we can use even more function magic.
  #r = random.randint
  rp = partial(random.randint, 1, 10000)

  # now we're ready to write our awesome query iterator thingy
  # first of all, we know the id's correspond to the random number we're picking, so we can create
  # dictionaries on the fly instead of serializing later
  # by creating dicts, we don't need to user the model serializer, which is probably slow and only appropriate
  # for complicated serializations of joins and crazy query sets etc
  # test xrange vs range if the query number is gigantic
  worlds = uj_dumps([{'id' : r, 'randomNumber' : g(id=r).randomnumber} for r in [rp() for q in xrange(queries)]])
  return HttpResponse(worlds, mimetype="application/json")

def fortunes(request):
  fortunes = list(Fortune.objects.all())
  fortunes.append(Fortune(id=0, message="Additional fortune added at request time."))

  fortunes = sorted(fortunes, key=attrgetter('message'))

  context = {'fortunes': fortunes}
  return render(request, 'fortunes.html', context)

def update(request):
  queries = _get_queries(request)
  g = World.objects.get
  rp = partial(random.randint, 1, 10000)
  
  worlds = []
  for r in [rp() for q in xrange(queries)]:
    w = g(id=r)
    w.randomnumber=rp()
    w.save()
    worlds.append({'id' : r, 'randomNumber' : w.randomnumber})

  return HttpResponse(uj_dumps(worlds), mimetype="application/json")
