# Falcon Benchmark Test (ported from Flask example)

Single file test, [app.py](app.py)

## Description

Falcon API framework (http://falconframework.org)

### Interpreter

* CPython 2.7
* CPython 3.3
* PyPy 2.0

### Database

(none at the moment)

### Server

* gunicorn+meinheld on CPython
* Tornado on PyPy

## Test URLs
### JSON Encoding

http://localhost:8080/json

### Plaintext

http://localhost:8080/plaintext
