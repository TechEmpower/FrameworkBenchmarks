import multiprocessing
import os
import sys

_is_travis = os.environ.get('TRAVIS') == 'true'

workers = multiprocessing.cpu_count() * 3
if _is_travis:
    workers = 2

bind = "0.0.0.0:8008"
keepalive = 120
errorlog = '-'
pidfile = 'api_hour.pid'
pythonpath = 'hello'