from webtest import TestApp as Client
import morepath

import app
from app import App


def setup_module(module):
    morepath.scan(app)
    morepath.commit(App)


def test_json():
    """/json"""
    app = App()
    c = Client(app)

    response = c.get('/json', status=200)
    assert response.headerlist == [
        ('Content-Type', 'application/json'),
        ('Content-Length', '27')
    ]
    assert response.json == {"message": "Hello, World!"}


def test_db():
    """/db"""
    app = App()
    c = Client(app)

    response = c.get('/db', status=200)
    assert response.content_type == 'application/json'
    assert 'id' in response.json
    assert 'randomNumber' in response.json
    assert 1 <= response.json['id'] <= 10000
    assert 1 <= response.json['randomNumber'] <= 10000


def test_queries():
    """/queries?queries="""
    app = App()
    c = Client(app)

    response = c.get('/queries?queries=', status=200)
    assert response.content_type == 'application/json'
    assert len(response.json) == 1


def test_queries_foo():
    """/queries?queries=foo"""
    app = App()
    c = Client(app)

    response = c.get('/queries?queries=foo', status=200)
    assert response.content_type == 'application/json'
    assert len(response.json) == 1


def test_queries_0():
    """/queries?queries=0"""
    app = App()
    c = Client(app)

    response = c.get('/queries?queries=0', status=200)
    assert response.content_type == 'application/json'
    assert len(response.json) == 1


def test_queries_999():
    """/queries?queries=999"""
    app = App()
    c = Client(app)

    response = c.get('/queries?queries=999', status=200)
    assert response.content_type == 'application/json'
    assert len(response.json) == 500


def test_queries_10():
    """/queries?queries=10"""
    app = App()
    c = Client(app)

    response = c.get('/queries?queries=10', status=200)
    assert response.content_type == 'application/json'
    assert len(response.json) == 10

    obj_list = response.json
    for obj in obj_list:
        assert 'id' in obj
        assert 'randomNumber' in obj
        assert 1 <= obj['id'] <= 10000
        assert 1 <= obj['randomNumber'] <= 10000


def test_fortunes():
    """/fortunes"""
    app = App()
    c = Client(app)

    response = c.get('/fortunes', status=200)
    assert response.headerlist == [
        ('Content-Type', 'text/html; charset=UTF-8'),
        ('Content-Length', '1304')
    ]
    assert response.text == fortunes


def test_updates():
    """/updates?queries="""
    app = App()
    c = Client(app)

    response = c.get('/updates?queries=', status=200)
    assert response.content_type == 'application/json'
    assert len(response.json) == 1


def test_updates_foo():
    """/updates?queries=foo"""
    app = App()
    c = Client(app)

    response = c.get('/updates?queries=foo', status=200)
    assert response.content_type == 'application/json'
    assert len(response.json) == 1


def test_updates_0():
    """/updates?queries=0"""
    app = App()
    c = Client(app)

    response = c.get('/updates?queries=0', status=200)
    assert response.content_type == 'application/json'
    assert len(response.json) == 1


def test_updates_999():
    """/updates?queries=999"""
    app = App()
    c = Client(app)

    response = c.get('/updates?queries=999', status=200)
    assert response.content_type == 'application/json'
    assert len(response.json) == 500


def test_updates_10():
    """/updates?queries=10"""
    app = App()
    c = Client(app)

    response = c.get('/updates?queries=10', status=200)
    assert response.content_type == 'application/json'
    assert len(response.json) == 10

    obj_list = response.json
    for obj in obj_list:
        assert 'id' in obj
        assert 'randomNumber' in obj
        assert 1 <= obj['id'] <= 10000
        assert 1 <= obj['randomNumber'] <= 10000


def test_plaintext():
    """/plaintext"""
    app = App()
    c = Client(app)

    response = c.get('/plaintext', status=200)
    assert response.headerlist == [
        ('Content-Type', 'text/plain; charset=UTF-8'),
        ('Content-Length', '13')
    ]
    assert response.text == 'Hello, World!'


fortunes = """<!DOCTYPE html>

<html>
<head>
<title>Fortunes</title>
</head>
<body>
<table>
<tr>
<th>id</th>
<th>message</th>
</tr>

<tr>
<td>11</td>
<td>&lt;script&gt;alert(&#34;This should not be displayed in a browser alert box.&#34;);&lt;/script&gt;</td>
</tr>

<tr>
<td>4</td>
<td>A bad random number generator: 1, 1, 1, 1, 1, 4.33e+67, 1, 1, 1</td>
</tr>

<tr>
<td>5</td>
<td>A computer program does what you tell it to do, not what you want it to do.</td>
</tr>

<tr>
<td>2</td>
<td>A computer scientist is someone who fixes things that aren&#39;t broken.</td>
</tr>

<tr>
<td>8</td>
<td>A list is only as strong as its weakest link. — Donald Knuth</td>
</tr>

<tr>
<td>0</td>
<td>Additional fortune added at request time.</td>
</tr>

<tr>
<td>3</td>
<td>After enough decimal places, nobody gives a damn.</td>
</tr>

<tr>
<td>7</td>
<td>Any program that runs right is obsolete.</td>
</tr>

<tr>
<td>10</td>
<td>Computers make very fast, very accurate mistakes.</td>
</tr>

<tr>
<td>6</td>
<td>Emacs is a nice operating system, but I prefer UNIX. — Tom Christaensen</td>
</tr>

<tr>
<td>9</td>
<td>Feature: A bug with seniority.</td>
</tr>

<tr>
<td>1</td>
<td>fortune: No such file or directory</td>
</tr>

<tr>
<td>12</td>
<td>フレームワークのベンチマーク</td>
</tr>

</table>
</body>
</html>"""
