import random
from operator import itemgetter
from functools import partial
from orjson import dumps

from django.db import connection
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
    row = World.objects.values_list('id', 'randomnumber').get(id=r)
    return HttpResponse(dumps({"id": row[0], "randomNumber": row[1]}), content_type="application/json")


def dbs(request):
    queries = _get_queries(request)
    result = []

    for _ in range(queries):
        r = _random_int()
        row = World.objects.values_list('id', 'randomnumber').get(id=r)
        result.append({"id": row[0], "randomNumber": row[1]})

    return HttpResponse(dumps(result), content_type="application/json")


def fortunes(request):
    fortunes = list(Fortune.objects.values("id", "message"))
    fortunes.append({"id": 0, "message": "Additional fortune added at request time."})
    fortunes.sort(key=itemgetter("message"))

    return render(request, "fortunes.html", {"fortunes": fortunes})


def update(request):
    queries = _get_queries(request)
    result = []
    updates = []


    for _ in range(queries):
        r = _random_int()
        World.objects.values_list('id', 'randomnumber').get(id=r)  # Required read
        new_number = _random_int()
        result.append({"id": r, "randomNumber": new_number})
        updates.append((new_number, r))

    # Batch update using raw SQL executemany
    with connection.cursor() as cursor:
        cursor.executemany(
            "UPDATE world SET randomnumber = %s WHERE id = %s",
            updates
        )

    return HttpResponse(dumps(result), content_type="application/json")
