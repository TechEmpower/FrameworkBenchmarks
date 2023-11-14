"""
Test views, per the spec here:
    http://www.techempower.com/benchmarks/#section=code&hw=i7&test=json
"""
from operator import itemgetter
from random import randint, sample

from pyramid.response import Response
from pyramid.view import view_config
from sqlalchemy import select
from sqlalchemy.orm.attributes import flag_modified

from frameworkbenchmarks.models import Fortune, World


def parse_query(request):
    queries = request.GET.get("queries", 1)
    try:
        queries = int(queries)
    except ValueError:
        queries = 1
    else:
        if queries < 1:
            queries = 1
        elif queries > 500:
            queries = 500
    return queries


@view_config(route_name="test_1", renderer="json")
def test_1(request):
    """
    Test type 1: JSON serialization
    """
    return {"message": "Hello, World!"}


@view_config(route_name="test_2", renderer="json")
def test_2(request):
    """
    Test type 2: Single database query
    """
    num = randint(1, 10000)
    result = request.dbsession.get(World, num)
    return result.__json__()


@view_config(route_name="test_3", renderer="json")
def test_3(request):
    """
    Test type 3: Multiple database queries
    """
    queries = parse_query(request)
    result = [
        request.dbsession.get(World, num).__json__()
        for num in sample(range(1, 10001), queries)
    ]
    return result


@view_config(route_name="test_4", renderer="templates/test_4.pt")
def test_4(request):
    """
    Test type 4: Fortunes
    """
    fortunes = (
        request.dbsession.execute(select(Fortune.id, Fortune.message)).mappings().all()
    )
    fortunes.append({"id": 0, "message": "Additional fortune added at request time."})
    fortunes.sort(key=itemgetter("message"))
    return {"fortunes": fortunes}


@view_config(route_name="test_5", renderer="json")
def test_5(request):
    """
    Test type 5: Database updates
    """
    queries = parse_query(request)
    resultset = []
    sess = request.dbsession
    for num in sample(range(1, 10001), queries):
        obj = sess.get(World, num)
        obj.randomNumber = randint(1, 10000)
        # force sqlalchemy to UPDATE entry even if the value has not changed
        # doesn't make sense in a real application, added only for pass `tfb verify`
        flag_modified(obj, "randomNumber")
        resultset.append(obj.__json__())
    sess.commit()
    return resultset


@view_config(route_name="test_6")
def test_6(request):
    """
    Test type 6: Plaintext
    """
    response = Response(
        body=b"Hello, World!", content_type="text/plain", charset="utf-8"
    )
    return response
