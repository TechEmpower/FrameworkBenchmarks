#!/usr/bin/env python
from datetime import datetime
from functools import lru_cache
import os
from random import randint, sample

from microdot_wsgi import Microdot
from microdot_jinja import render_template
import psycopg2
from psycopg2.extras import execute_batch
from cachetools import cached
from cachetools.keys import hashkey

app = Microdot()
db = psycopg2.connect(os.environ['DATABASE_URL'])

get_world_sql = 'SELECT id, randomnumber FROM world WHERE id = $1'
update_world_sql = 'UPDATE world SET randomnumber = $1 WHERE id = $2'
fortune_sql = 'SELECT * FROM fortune'
with db.cursor() as cur:
    cur.execute('PREPARE get_world AS ' + get_world_sql)
    cur.execute('PREPARE update_world AS ' + update_world_sql)
    cur.execute('PREPARE fortune AS ' + fortune_sql)


def get_num_queries(request, name="queries"):
    try:
        num_queries = request.args.get(name, 1, type=int)
    except ValueError:
        num_queries = 1
    if num_queries < 1:
        return 1
    if num_queries > 500:
        return 500
    return num_queries


def generate_ids(num_queries):
    return sample(range(1, 10001), num_queries)


@app.route("/json")
def test_json(request):
    return {"message": "Hello, World!"}


@app.route("/db")
def test_db(request):
    id = randint(1, 10000)
    with db.cursor() as cur:
        cur.execute('EXECUTE get_world (%s)', (id,))
        result = cur.fetchone()
        world = {'id': result[0], 'randomNumber': result[1]}
    return world


def get_world(cur, id):
    cur.execute('EXECUTE get_world (%s)', (id,))
    result = cur.fetchone()
    return {'id': result[0], 'randomNumber': result[1]}


@app.route("/queries")
def test_queries(request):
    with db.cursor() as cur:
        worlds = [get_world(cur, id) for id in generate_ids(get_num_queries(request))]
    return worlds


@app.route("/fortunes")
def test_fortunes(request):
    with db.cursor() as cur:
        cur.execute('EXECUTE fortune')
        fortunes = list(cur.fetchall())
    fortunes.append((0, 'Additional fortune added at request time.'))
    fortunes.sort(key=lambda f: f[1])
    return render_template("fortunes_raw.html", fortunes=fortunes), {'Content-Type': 'text/html; charset=utf-8'}


@app.route("/updates")
def test_updates(request):
    worlds = []
    updated_worlds = []
    with db.cursor() as cur:
        for id in generate_ids(get_num_queries(request)):
            cur.execute('EXECUTE get_world (%s)', (id,))
            result = cur.fetchone()
            new_value = randint(1, 10000)
            updated_worlds.append((new_value, result[0]))
            worlds.append({'id': result[0], 'randomNumber': new_value})
        execute_batch(cur, 'EXECUTE update_world (%s, %s)', updated_worlds)
        db.commit()
    return worlds


@app.route("/plaintext")
def test_plaintext(request):
    return b"Hello, World!"


@cached(cache={}, key=lambda cur, id: hashkey(id))
def get_cached_world(cur, id):
    cur.execute('EXECUTE get_world (%s)', (id,))
    result = cur.fetchone()
    return {'id': result[0], 'randomNumber': result[1]}


@app.route("/cached-queries")
def test_cached_queries(request):
    with db.cursor() as cur:
        worlds = [get_cached_world(cur, id) for id in generate_ids(get_num_queries(request, 'count'))]
    return worlds
