#!/usr/bin/env python
import multiprocessing
from itertools import repeat
from operator import itemgetter

from flask import Flask, Response

from helpers import *

# setup



app = Flask(__name__)
(
    POOL,
    TEMPLATE,
    READ_ROW_SQL,
    PREPARED_READ_ROW_SQL,
    WRITE_ROW_SQL,
    ADDITIONAL_ROW,
) = setup(multiprocessing.cpu_count() * 2.5)


@app.route("/json")
def hello():
    return orjson.dumps({"message": "Hello, World!"}), {
        "Content-Type": "application/json"
    }


@app.route("/query")
def get_random_world():
    db = POOL.getconn()
    cursor = db.cursor()
    num_queries = get_num_queries()
    results = map(query, zip(repeat(cursor, num_queries), generate_ids(num_queries)))
    worlds = [{"id": result[0], "randomNumber": result[1]} for result in results]
    POOL.putconn(db)
    return orjson.dumps(worlds), {"Content-Type": "application/json"}


@app.route("/db")
def get_random_world_single():
    db = POOL.getconn()
    cursor = db.cursor()
    cursor.execute("EXECUTE read_stmt(%s)", generate_ids(1))
    result = cursor.fetchone()
    world = {"id": result[0], "randomNumber": result[1]}
    POOL.putconn(db)
    return orjson.dumps(world), {"Content-Type": "application/json"}


@app.route("/fortunes")
def get_fortunes():
    db = POOL.getconn()
    cursor = db.cursor()
    cursor.execute("EXECUTE fortune")
    fortunes = list(cursor.fetchall())
    fortunes.append(ADDITIONAL_ROW)
    fortunes.sort(key=itemgetter(1))
    POOL.putconn(db)
    return Response(TEMPLATE.render(fortunes=fortunes))


@app.route("/updates")
def updates():
    """Test 5: Database Updates"""
    db = POOL.getconn()
    cursor = db.cursor()
    num_queries = get_num_queries()
    ids = generate_ids(num_queries)
    update_values = generate_ids(num_queries)
    list(map(query, zip(repeat(cursor, num_queries), generate_ids(num_queries))))
    worlds = list(zip(ids, update_values))
    execute_batch(cursor, "EXECUTE write_stmt(%s, %s)", worlds)
    db.commit()
    POOL.putconn(db)
    return orjson.dumps(
        [{"id": ident, "randomNumber": update} for ident, update in worlds]
    ), {"Content-Type": "application/json"}


@app.route("/plaintext")
def plaintext():
    """Test 6: Plaintext"""
    return b"Hello, World!", {"Content-Type": "text/plain"}


# entry point for debugging
if __name__ == "__main__":
    app.run(debug=True)
