#!/usr/bin/env python
from flask import Flask, make_response, jsonify
import os
import multiprocessing
import logging
from socketify import WSGI
# setup
app = Flask(__name__)
app.config["JSONIFY_PRETTYPRINT_REGULAR"] = False


@app.route("/json")
def hello():
    return jsonify(message="Hello, World!")

@app.route("/plaintext")
def plaintext():
    """Test 6: Plaintext"""
    response = make_response(b"Hello, World!")
    response.content_type = "text/plain"
    return response



_is_travis = os.environ.get('TRAVIS') == 'true'

workers = int(multiprocessing.cpu_count())
if _is_travis:
    workers = 2


def run_app():
    WSGI(app).listen(8080, lambda config: logging.info(f"Listening on port http://localhost:{config.port} now\n")).run()


def create_fork():
    n = os.fork()
    # n greater than 0 means parent process
    if not n > 0:
        run_app()


# fork limiting the cpu count - 1
for i in range(1, multiprocessing.cpu_count()):
    create_fork()

run_app()  # run app on the main process too :)
