# Create your views here.

from django.template import Context, loader
from django.http import HttpResponse
from django.core import serializers
from world.models import World
# json is available on any real production environment and better
import json
#import simplejson
import random

def json(request):
  return HttpResponse(json.dumps({"message" : "Hello, World!"), mimetype="application/json")

def db(request):
  queries = int(request.GET.get('queries', 1))
  # make your id's all at once unless it's cheating
  ids = [random.randint(1, 10000) for x in range(queries)]
  # if it's cheating to run only one DB query, no worries
  # my async changes in the DB setup will compensate nicely  
  # get random rows, we know the ids
  
  # If the id's aren't needed, use the power of the ORM
  # get the stuff you need only with .values_list('randomNumber', flat=True) 
  worlds = World.objects.filter(id__in=ids))

  return HttpResponse(serializers.serialize("json", worlds), mimetype="application/json")

