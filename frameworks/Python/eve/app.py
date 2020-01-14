# -*- coding: utf-8 -*-

from eve import Eve
from flask import request
import json

port = 8080
host = '127.0.0.1'

app = Eve()

@app.after_request
def add_correct_headers(response):
    path = request.path
    if path == '/json':
        response.headers['Content-Type'] = 'application/json'
    elif path == '/plaintext':
        response.headers['Content-Type'] = 'text/plain'
    return response

@app.route('/plaintext')
def hello_world():
    return 'Hello, World!'

@app.route('/json')
def jsonHello():
    hello = {'message': 'Hello, World!'}
    return json.dumps(hello)

if __name__ == '__main__':
    app.run(host=host, port=port)
