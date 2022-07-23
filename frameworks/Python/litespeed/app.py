import multiprocessing
import sys
from _operator import itemgetter
from html import escape
from itertools import repeat
from random import randint

from litespeed import route, start_with_args, register_error_page, render
from litespeed.utils import Request
from pymysql.cursors import Cursor
from pymysqlpool import ConnectionPool


THREADS = multiprocessing.cpu_count() * 2.5
POOL = ConnectionPool(size=int(THREADS / 4), maxsize=int(THREADS / 4), database="hello_world", user="benchmarkdbuser", password="benchmarkdbpass", host="tfb-database")


def get_num_queries(request: Request) -> int:
    try:
        num_queries = int(request['GET'].get("queries", 1))
    except ValueError:
        num_queries = 1
    if num_queries < 1:
        return 1
    if num_queries > 500:
        return 500
    return num_queries


def world_query(args: tuple[Cursor, int]) -> tuple:
    cursor, id = args
    cursor.execute('SELECT randomnumber, id FROM world WHERE id = %s', (id,))
    return cursor.fetchone()


def generate_ids(num_queries: int) -> set[int]:
    ids = {randint(1, 10000) for _ in range(num_queries)}
    while len(ids) < num_queries:
        ids.add(randint(1, 10000))
    return ids


@route("/json")
def hello(request: Request):
    """Test 1: JSON Serialization"""
    return {"message": "Hello, World!"}, 200, {"Server": "LiteSpeed/1.2.0"}


@route("/query")
def get_random_world(request: Request):
    """Test 2: Single Database Query"""
    num_queries = get_num_queries(request)
    with POOL.get_connection() as db:
        cursor = db.cursor()
        results = map(world_query, zip(repeat(cursor, num_queries), generate_ids(num_queries)))
        worlds = [{"id": result[0], "randomNumber": result[1]} for result in results]
    return worlds, 200, {"Server": "LiteSpeed/1.2.0"}


@route("/db")
def get_random_world_single(request: Request):
    """Test 3: Multiple Database Queries"""
    with POOL.get_connection() as db:
        cursor = db.cursor()
        result = world_query((cursor, randint(1, 10000)))
    return {"id": result[0], "randomNumber": result[1]}, 200, {"Server": "LiteSpeed/1.2.0"}


@route("/fortunes")
def get_fortunes(request: Request):
    """Test 4: Fortunes"""
    with POOL.get_connection() as db:
        cursor = db.cursor()
        cursor.execute("SELECT * FROM Fortune")
        fortunes = list(cursor.fetchall())
        fortunes.append((0, "Additional fortune added at request time."))
        fortunes.sort(key=itemgetter(1))
    fortunes = ''.join(f'<tr><td>{id}</td><td>{escape(message)}</td></tr>' for id, message in fortunes)
    body, status, headers = render(request, 'fortunes.html', data={'fortunes': fortunes})
    headers['Content-Type'] += '; charset=utf-8'
    headers['Server'] = 'LiteSpeed/1.2.0'
    return body, status, headers


@route("/updates")
def updates(request: Request):
    """Test 5: Database Updates"""
    with POOL.get_connection() as db:
        cursor = db.cursor()
        num_queries = get_num_queries(request)
        ids = generate_ids(num_queries)
        update_values = generate_ids(num_queries)
        list(map(world_query, zip(repeat(cursor, num_queries), generate_ids(num_queries))))
        worlds = list(zip(ids, update_values))
        cursor.executemany('UPDATE world SET randomnumber=%s WHERE id=%s', worlds)
        db.commit()
    return [{"id": ident, "randomNumber": update} for ident, update in worlds], 200, {"Server": "LiteSpeed/1.2.0"}


@route("/plaintext")
def plaintext(request: Request):
    """Test 6: Plaintext"""
    return "Hello, World!", 200, {"Content-Type": "text/plain", "Server": "LiteSpeed/1.2.0"}


if __name__ == '__main__':
    start_with_args()