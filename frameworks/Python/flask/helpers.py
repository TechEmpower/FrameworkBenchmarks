import os
import sys
from collections import namedtuple
from random import randint

import jinja2
from flask import request


if sys.version_info[0] == 3:
    xrange = range

_is_pypy = hasattr(sys, "pypy_version_info")
if _is_pypy:
    from psycopg2cffi.pool import ThreadedConnectionPool
    from psycopg2cffi.extras import execute_batch
    import ujson as orjson
else:
    from psycopg2.pool import ThreadedConnectionPool
    from psycopg2.extras import execute_batch
    import orjson
    try:
        import meinheld
        import meinheld.patch

        meinheld.server.set_access_logger(None)
        meinheld.set_keepalive(30)
        meinheld.patch.patch_all()
    except ImportError:
        pass


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


def load_fortunes_template():
    path = os.path.join("templates", "fortune_raw.html")
    with open(path, "r") as template_file:
        template_text = template_file.read()
        return jinja2.Template(template_text)


def setup(threads):
    pool = ThreadedConnectionPool(
        minconn=int(threads / 4),
        maxconn=int(threads / 4),
        database="hello_world",
        user="benchmarkdbuser",
        password="benchmarkdbpass",
        host="tfb-database",
        port=5432,
    )
    template = load_fortunes_template()
    read_row_sql = (
        'SELECT world."randomnumber", world."id" FROM "world" WHERE id = %(id)s'
    )
    prepared_read_row_sql = (
        'SELECT world."randomnumber", world."id" FROM "world" WHERE id = $1'
    )
    write_row_sql = 'UPDATE "world" SET "randomnumber"=$1 WHERE id=$2'
    additional_row = (0, "Additional fortune added at request time.")
    db = pool.getconn()
    cursor = db.cursor()
    cursor.execute("PREPARE read_stmt (int) AS " + prepared_read_row_sql)
    cursor.execute("PREPARE write_stmt (int, int) AS " + write_row_sql)
    cursor.execute('PREPARE fortune AS SELECT * FROM "Fortune"')
    del cursor
    pool.putconn(db)
    del db
    return (
        pool,
        template,
        read_row_sql,
        prepared_read_row_sql,
        write_row_sql,
        additional_row,
    )


def query(arg):
    (cursor, ident) = arg
    cursor.execute("EXECUTE read_stmt(%s)", [ident])
    result = cursor.fetchone()
    return result


Fortune = namedtuple("Fortune", ["id", "message"])
