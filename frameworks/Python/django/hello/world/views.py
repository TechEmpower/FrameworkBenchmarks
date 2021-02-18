import random
from operator import itemgetter
from functools import partial
from ujson import dumps as uj_dumps

from django.http import HttpResponse
from django.shortcuts import render

from world.models import World, Fortune


_random_int = partial(random.randint, 1, 10000)


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


def plaintext(request):
    return HttpResponse("Hello, World!", content_type="text/plain")


def json(request):
    return HttpResponse(
            uj_dumps({"message": "Hello, World!"}),
            content_type="application/json"
        )


def db(request):
    r = _random_int()
    world = uj_dumps({
        'id': r,
        'randomNumber': World.objects.get(id=r).randomnumber
    })
    return HttpResponse(world, content_type="application/json")


def dbs(request):
    queries = _get_queries(request)

    def caller(input_):
        int_ = _random_int()
        return {'id': int_, 'randomNumber': World.objects.get(id=int_).randomnumber}
    worlds = tuple(map(caller, range(queries)))

    return HttpResponse(uj_dumps(worlds), content_type="application/json")


def fortunes(request):
    fortunes = list(Fortune.objects.values('id', 'message'))
    fortunes.append({"id": 0, 'message': "Additional fortune added at request time."})
    fortunes.sort(key=itemgetter('message'))

    return render(request, 'fortunes.html', {'fortunes': fortunes})


def update(request):
    queries = _get_queries(request)

    def caller(input_):
        w = World.objects.get(id=_random_int())
        w.randomnumber = _random_int()
        w.save()
        return {'id': w.id, 'randomNumber': w.randomnumber}
    worlds = tuple(map(caller, range(queries)))

    return HttpResponse(uj_dumps(worlds), content_type="application/json")
