# Python frameworks

The information below contains information specific to Python. 
For further guidance, review the 
[documentation](https://github.com/TechEmpower/FrameworkBenchmarks/wiki).

## Infrastructure Software Versions

* [CPython3](https://www.python.org/)
* [CPython2](https://www.python.org/)
* [PyPy3](http://pypy.org/)
* [PyPy2](http://pypy.org/)
* [nginx](http://nginx.org/)
* [uWSGI](https://uwsgi-docs.readthedocs.org/en/latest/)

## Get Help

### Python Experts

* INADA Naoki (@methane) -- CPython core developer and expert of Python's MySQL driver.
* Ludovic Gasc (@GMLudo) -- AsyncIO and aiohttp user.

### [Python Community](https://www.python.org/community/)

* `#python` IRC Channel ([irc.freenode.net](http://freenode.net/))
* `#python-dev` IRC Channel ([irc.freenode.net](http://freenode.net/))

## Python interpreters

### (C)Python 3

Newest Python.

It is the most major interpreter for Web Development.

### (C)Python 2

Legacy Python. Still used for widely.

### PyPy3

PyPy is the fastest Python implementation with JIT.

### PyPy2

Legacy PyPy. Still used for widely.

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

Consider Python 3 first. -- Python 3 is mainstream for web development.