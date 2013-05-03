# Create your views here.

from django.template import Context, loader
from django.http import HttpResponse
from django.core import serializers
from world.models import World, Fortune
from django.shortcuts import render
import ujson
import random
from operator import attrgetter
import numpy.random as nprnd
from functools import partial

def json(request):
  response = {
    "message": "Hello, World!"
  }
  return HttpResponse(ujson.dumps(response), mimetype="application/json")

def db(request):
  queries = int(request.GET.get('queries', 1))
  # worlds = []
  # it's not required to explicitly loop instead of using list comprehension
  #for i in range(queries):
    # get a random row, we know the ids are between 1 and 10000
    #worlds.append(World.objects.get(id=random.randint(1, 10000)))
  # instead we can do:
  #worlds = [World.objects.get(id=random.randint(1, 10000)) for i in range(queries)]
  
  # fun fact:  every dot-notation lookup calls some python magic under the hood.  Like every other code,
  # one can eliminate dereferences by storing the end dereferenced thing in an identifier
  g = World.objects.get
  #r = random.randint
  # but wait! there's more!
  #http://stackoverflow.com/questions/4172131/create-random-list-of-integers-in-python
  #r = nprnd.randint
  # but wait!  there's more!  if we're calling a function over and over with the same parameters, 
  # we can use even more function magic.
  rp = partial(nprnd.randint, 10000)
  # now we're ready to write our awesome query iterator thingy
  # first of all, we know the id's correspond to the random number we're picking, so we can create
  # dictionaries on the fly instead of serializing later
  # by creating dicts, we don't need to user the model serializer, which is probably slow and only appropriate
  # for complicated serializations of joins and crazy query sets etc
  # test xrange vs range if the query number is gigantic
  worlds = ujson.dumps([{'id' : r, 'randomNumber' : g(id=r).randomnumber} for r in [rp() for q in xrange(queries)]])  
  return HttpResponse(worlds, mimetype="application/json")

def fortunes(request):
  fortunes = list(Fortune.objects.all())
  fortunes.append(Fortune(id=0, message="Additional message added at runtime."))

  fortunes = sorted(fortunes, key=attrgetter('message'))

  context = {'fortunes': fortunes}
  return render(request, 'fortunes.html', context)
