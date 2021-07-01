#!/usr/bin/env python
from collections import namedtuple
from operator import attrgetter, itemgetter
from random import randint
import sys

from flask import Flask, request, render_template, make_response, jsonify


if sys.version_info[0] == 3:
    xrange = range
_is_pypy = hasattr(sys, 'pypy_version_info')
if _is_pypy:
    from psycopg2cffi.pool import ThreadedConnectionPool
else:
    from psycopg2.pool import ThreadedConnectionPool
# setup

app = Flask(__name__)

pool = ThreadedConnectionPool(minconn=2, maxconn=2, database="hello_world", user="benchmarkdbuser", password="benchmarkdbpass", host="tfb-database", port=5432)

READ_ROW_SQL = 'SELECT world."randomnumber", world."id" FROM "world" WHERE id = %(id)s'
WRITE_ROW_SQL = 'UPDATE "world" SET "randomnumber"=%(randomNumber)s WHERE id=%(id)s'

def get_num_queries():
    try:
        num_queries = request.args.get("queries", 1, type=int)
    except ValueError:
        num_queries = 1
    if num_queries < 1:
        return 1
    if num_queries > 500:
        return 500
    return num_queries


def generate_ids(num_queries):
    ids = {randint(1, 10000) for _ in xrange(num_queries)}
    while len(ids) < num_queries:
        ids.add(randint(1, 10000))
    return list(sorted(ids))


@app.route("/json")
def hello():
    return jsonify(message='Hello, World!')


@app.route("/query")
def get_random_world():
    db = pool.getconn()
    cursor = db.cursor()
    def query(ident):
        cursor.execute(READ_ROW_SQL, {"id": ident})
        result = cursor.fetchone()
        return result
    worlds = [{'id': result[0], 'randomNumber': result[1]} for result in map(query, generate_ids(get_num_queries()))]
    pool.putconn(db)
    return jsonify(worlds)


@app.route("/db")
def get_random_world_single():
    db = pool.getconn()
    cursor = db.cursor()
    cursor.execute(READ_ROW_SQL, {"id": randint(1, 10000)})
    result = cursor.fetchone()
    world = {'id': result[0], 'randomNumber': result[1]}
    pool.putconn(db)
    return jsonify(world)


Fortune = namedtuple("Fortune", ["id", "message"])


@app.route("/fortunes")
def get_fortunes():
    db = pool.getconn()
    cursor = db.cursor()
    cursor.execute('SELECT * FROM "Fortune"')
    fortunes = [Fortune(id=row[0], message=row[1]) for row in cursor.fetchall()]
    fortunes.append(Fortune(id=0, message="Additional fortune added at request time."))
    fortunes.sort(key=attrgetter('message'))
    pool.putconn(db)
    return render_template('fortunes.html', fortunes=fortunes)


@app.route("/updates")
def updates():
    """Test 5: Database Updates"""
    db = pool.getconn()
    cursor = db.cursor()
    num_queries = get_num_queries()
    ids = generate_ids(num_queries)
    updates = generate_ids(num_queries)
    def query(ident):
        cursor.execute(READ_ROW_SQL, {"id": ident})
        result = cursor.fetchone()
        return result
    list(map(query, generate_ids(get_num_queries())))
    worlds = [{"id": ident, "randomNumber": update} for ident, update in zip(ids, updates)]
    for world in worlds:
        cursor.execute(WRITE_ROW_SQL, world)
    db.commit()
    pool.putconn(db)
    return jsonify(worlds)


@app.route('/plaintext')
def plaintext():
    """Test 6: Plaintext"""
    response = make_response(b'Hello, World!')
    response.content_type = 'text/plain'
    return response


try:
    import meinheld
    meinheld.server.set_access_logger(None)
    meinheld.set_keepalive(120)
except ImportError:
    pass

# entry point for debugging
if __name__ == "__main__":
    app.run(debug=True)
