import random
from operator import itemgetter
from functools import partial
from orjson import dumps

from django.http import HttpResponse
from django.shortcuts import render

from world.models import World, Fortune


_random_int = partial(random.randint, 1, 10000)


def _get_queries(request):
    try:
        queries = int(request.GET.get("queries", 1))
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
        dumps({"message": "Hello, World!"}), content_type="application/json"
    )


def db(request):
    r = _random_int()
    world = dumps({"id": r, "randomNumber": World.objects.get(id=r).randomnumber})
    return HttpResponse(world, content_type="application/json")


def dbs(request):
    queries = _get_queries(request)
    ids = [_random_int() for _ in range(queries)]

    # Batch fetch with only needed fields
    worlds = World.objects.filter(id__in=ids).only('id', 'randomnumber')

    result = [{"id": w.id, "randomNumber": w.randomnumber} for w in worlds]
    return HttpResponse(dumps(result), content_type="application/json")


def fortunes(request):
    fortunes = list(Fortune.objects.values("id", "message"))
    fortunes.append({"id": 0, "message": "Additional fortune added at request time."})
    fortunes.sort(key=itemgetter("message"))

    return render(request, "fortunes.html", {"fortunes": fortunes})


def update(request):
    queries = _get_queries(request)
    ids = [_random_int() for _ in range(queries)]

    # Batch fetch
    worlds = list(World.objects.filter(id__in=ids))

    # Update in memory
    for world in worlds:
        world.randomnumber = _random_int()

    # Batch save
    World.objects.bulk_update(worlds, ['randomnumber'])

    result = [{"id": w.id, "randomNumber": w.randomnumber} for w in worlds]
    return HttpResponse(dumps(result), content_type="application/json")
