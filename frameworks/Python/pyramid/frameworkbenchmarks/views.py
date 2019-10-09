"""
Test views, per the spec here:
    http://www.techempower.com/benchmarks/#section=code&hw=i7&test=json
"""
from operator import itemgetter
from random import randint
import sys

from pyramid.view import view_config
from pyramid.response import Response
from frameworkbenchmarks.models import DBSession, World, Fortune
from sqlalchemy.ext import baked

if sys.version_info[0] == 3:
    xrange = range

bakery = baked.bakery()


@view_config(route_name='test_1', renderer='json')
def test_1(request):
    """
    Test type 1: JSON serialization
    """
    return {"message": "Hello, World!"}


@view_config(route_name='test_2', renderer='json')
def test_2(request):
    """
    Test type 2: Single database query
    """
    num = randint(1, 10000)
    baked_query = bakery(lambda session: session.query(World))
    result = baked_query(DBSession).get(num)
    return result.__json__()


@view_config(route_name='test_3', renderer='json')
def test_3(request):
    """
    Test type 3: Multiple database queries
    """
    queries = request.GET.get('queries', 1)
    try:
        queries = int(queries)
    except ValueError:
        queries = 1
    else:
        if queries < 1:
            queries = 1
        elif queries > 500:
            queries = 500
    baked_query = bakery(lambda session: session.query(World))
    result = [
        baked_query(DBSession).get(num).__json__()
        for num in (randint(1, 10000) for _ in xrange(queries))
    ]
    return result


@view_config(route_name='test_4', renderer='templates/test_4.pt')
def test_4(request):
    """
    Test type 4: Fortunes
    """
    baked_query = bakery(lambda session: session.query(Fortune))

    fortunes = [obj.__json__() for obj in baked_query(DBSession).all()]
    fortunes.append(
        {"id": 0, "message": "Additional fortune added at request time."}
    )
    fortunes.sort(key=itemgetter('message'))
    return {'fortunes': fortunes}


@view_config(route_name='test_5', renderer='json')
def test_5(request):
    """
    Test type 5: Database updates
    """
    queries = request.GET.get('queries', 1)
    try:
        queries = int(queries)
    except ValueError:
        queries = 1
    else:
        if queries < 1:
            queries = 1
        elif queries > 500:
            queries = 500
    baked_query = bakery(lambda session: session.query(World))
    resultset = []
    for num in sorted(randint(1, 10000) for _ in xrange(queries)):
        obj = baked_query(DBSession).get(num)
        obj.randomNumber = randint(1, 10000)
        resultset.append(obj.__json__())
    DBSession.commit()
    return resultset


@view_config(route_name='test_6')
def test_6(request):
    """
    Test type 6: Plaintext
    """
    response = Response(
        body=b'Hello, World!',
        content_type='text/plain',
        )
    return response
