# Python frameworks

## Experts

* INADA Naoki (@methane) -- Expert of Python and Python's MySQL driver.


## Python interpreters

### (C)Python 3

Newest Python.

It will be most major interpreter for Web Development in this or next year.
This is a bit slower than CPython 2, but more simple and productive than it.

### (C)Python 2

Legacy Python.

It is most major interpreter for now.

### PyPy

PyPy is the fastest Python implementation with JIT.

There is PyPy's Python 3 implementation (PyPy3), but it is not so tuned like PyPy2.
So we don't use it for now.


## WSGI Servers

### Nginx + uWSGI

This is one of fast and realistic way to serve Python web application.

Use unix domain socket between nginx and uWSGI to avoid additional TCP/IP overhead.


### Gunicorn + Meinheld

[Meinheld](https://github.com/mopemope/meinheld) is very fast WSGI server.

Since Meinheld is safe against slowloris and support Keep-Alive, you can use it
without buffered HTTP reverse proxy (like nginx).

We use meinheld to measure bare (without DB access) performance of framework without
overhead of reverse proxying. (plaintext, json test)

Meinheld does not provide worker process management.
[Gunicorn](http://gunicorn.org/) provide it for Meinheld.


### Gunicorn + Tornado

uWSGI + PyPy is difficult to setup.
Meinheld doesn't so fast with PyPy because it uses Python/C API heavily.
So we use Tornado as HTTP/WSGI server.

It supports keep-alive. So it have nice performance about json or plaintext benchmark.


## Before writing new tests.

**Don't increase matrix without significant purpose.**

We can't maintain matrix of frameworks (5~) * servers (3~) * DBs (3) * Interpreters (3).

If you want to know about performance difference between MySQL and PostgreSQL,
there are no need to all frameworks implement both of them.
Additionally, Python is not a good language to measure performance of DBs because
web application written in Python is slower than DBs.

If you want to benchmark http server, you should not port all tests or frameworks.
Simple json or plaintext test is enough.

If your framework uses SQLAlchemy, Flask may be enough to know performance of SQLAlchemy.


## Advice when writing new tests

### Interpreter

Consider Python 3 first. -- Python 3 will be mainstream for web development soon.

### Server

Try Gunicorn + Meinheld first. All WSGI apps in this benchmark uses it. You can compare
your framework performance with others.

### Files you should provide

You can see Flask's files to know how to write new test.

`requirements.txt` is standard way to define depending libraries.

`install.sh` is executed before running test.  You should create virtualenv on `$TROOT/py2`
(or py3 for Python 3 and pypy for PyPy).  Then `$TROOT/py2/bin/pip install -r requirements.txt`.
virtualenv is installed on Python 2 and PyPy.  Use `$IROOT/py3/bin/python3 -m venv $TROOT/py3`
for Python 3.

`bechmark_config` is json file to define test.
See [here](https://github.com/TechEmpower/FrameworkBenchmarks#the-benchmark_config-file)

`setup_py2.py` is used to run test on Python 2.  `gunicorn_conf.py` is configuration for gunicorn.
`setup_py2.py` and `gunicorn_conf.py` are written as generic as possible.
You may be able to use it with changing wsgi callable.  (`$PY2_GUNICORN wsgimodule:callable -c gunicorn_conf.py`)
