#!/usr/bin/env python
from flask import Flask, make_response, jsonify
import os
import multiprocessing
import logging
import fastwsgi

# setup
app = Flask(__name__)
app.config["JSONIFY_PRETTYPRINT_REGULAR"] = False


@app.route("/json")
def hello():
    response = make_response(jsonify(message="Hello, World!"))
    response.content_type = "application/json"
    response.headers.set('Server', 'FastWSGI+Flask')
    return response

@app.route("/plaintext")
def plaintext():
    response = make_response(b"Hello, World!")
    response.content_type = "text/plain"
    response.headers.set('Server', 'FastWSGI+Flask')
    return response


_is_travis = os.environ.get('TRAVIS') == 'true'

workers = int(multiprocessing.cpu_count())
if _is_travis:
    workers = 2

host = '0.0.0.0'
port = 8080

def run_app():
    fastwsgi.run(app, host, port, loglevel=0)

def create_fork():
    n = os.fork()
    # n greater than 0 means parent process
    if not n > 0:
        run_app()

# fork limiting the cpu count - 1
for i in range(1, workers):
    create_fork()

run_app()  # run app on the main process too :)
