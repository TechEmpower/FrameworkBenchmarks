from random import randint

from .app import App
from .model import Json, World, WorldQueries, WorldUpdates, Plaintext
from .collection import FortuneCollection


@App.json(model=Json)
def test_1(self, request):
    """Test 1: JSON serialization"""
    return {'message': 'Hello, World!'}


@App.json(model=World)
def test_2(self, request):
    """Test 2: Single database query"""
    return {'id': self.id, 'randomNumber': self.randomnumber}


@App.json(model=WorldQueries)
def test_3(self, request):
    """Test 3: Multiple database queries"""
    try:
        queries = int(self.queries)
    except ValueError:
        queries = 1
    else:
        if queries < 1:
            queries = 1
        elif queries > 500:
            queries = 500

    result = []

    for id_ in [randint(1, 10000) for _ in range(queries)]:
        result.append({'id': id_, 'randomNumber': World[id_].randomnumber})

    return result


@App.html(model=FortuneCollection, template='fortune.jinja2')
def test_4(self, request):
    """Test 4: Fortunes"""
    fortunes = [f.to_dict() for f in self.query()]
    fortunes.append({
        'id': 0,
        'message': 'Additional fortune added at request time.'
    })

    return {'fortunes': sorted(fortunes, key=lambda x: x['message'])}


@App.json(model=WorldUpdates)
def test_5(self, request):
    """Test 5: Database updates"""
    try:
        queries = int(self.queries)
    except ValueError:
        queries = 1
    else:
        if queries < 1:
            queries = 1
        elif queries > 500:
            queries = 500

    result = []

    for id_ in sorted(randint(1, 10000) for _ in range(queries)):
        randomNumber = randint(1, 10000)
        World[id_].randomnumber = randomNumber
        result.append({'id': id_, 'randomNumber': randomNumber})

    return result


@App.view(model=Plaintext)
def test_6(self, request):
    """Test 6: Plaintext"""
    return 'Hello, World!'
