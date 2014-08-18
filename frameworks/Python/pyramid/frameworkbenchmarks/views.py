"""
Test views, per the spec here:
    http://www.techempower.com/benchmarks/#section=code&hw=i7&test=json
"""

from random import randint
from pyramid.view import view_config
from frameworkbenchmarks.models import DBSession, World, Fortune


@view_config(route_name='test_1', renderer='json')
def test_1(request):
    """
    Test type 1: JSON serialization
    """
    return {"message":"Hello, World!"}


@view_config(route_name='test_2', renderer='sqla_json')
def test_2(request):
    """
    Test type 2: Single database query
    """
    num = randint(1, 10001)
    result = DBSession.query(World).filter(World.id == num).one()
    return result


@view_config(route_name='test_3', renderer='sqla_json')
def test_3(request):
    """
    Test type 3: Multiple database queries
    """
    queries = request.params['queries']
    try:
        queries = int(queries)
    except ValueError:
        queries = 1
    else:
        if queries < 1:
            queries = 1
        elif queries > 500:
            queries = 500
    result = [
        DBSession.query(World).filter(World.id == num).one()
        for num in [randint(1, 10001) for _ in range(1, queries + 1)]
    ]
    return result


@view_config(route_name='test_4', renderer='templates/test_4.pt')
def test_4(request):
    """
    Test type 4: Fortunes
    """
    fortunes = [obj.__json__() for obj in DBSession.query(Fortune).all()]
    fortunes.append(
        {"id": 0, "message": "Additional fortune added at request time."}
    )
    return {'fortunes': sorted(fortunes, key=lambda x: x['message'])}


@view_config(route_name='test_5', renderer='json')
def test_5(request):
    """
    Test type 5: Database updates
    """
    queries = request.params['queries']
    try:
        queries = int(queries)
    except ValueError:
        queries = 1
    else:
        if queries < 1:
            queries = 1
        elif queries > 500:
            queries = 500
    objset = [
        DBSession.query(World).filter(World.id == num).one()
        for num in [randint(1, 10001) for _ in range(1, queries + 1)]
    ]
    for obj in objset:
        obj.randomNumber = randint(1, 10001)
    resultset = [obj.__json__() for obj in objset]
    DBSession.commit()
    return resultset


@view_config(route_name='test_6')
def test_6(request):
    """
    Test type 6: Plaintext
    """
    response = request.response
    response.text = u"Hello, World!" # py2k/3k
    response.content_type = "text/plain"
    return response
