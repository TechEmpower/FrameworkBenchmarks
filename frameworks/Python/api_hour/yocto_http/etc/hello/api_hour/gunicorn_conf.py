import multiprocessing
import os

_is_travis = os.environ.get('TRAVIS') == 'true'

workers = multiprocessing.cpu_count()
if _is_travis:
    workers = 2

bind = ['0.0.0.0:8080', '0.0.0.0:8081', '0.0.0.0:8082']
keepalive = 120
errorlog = '-'
pidfile = '/tmp/api_hour.pid'
pythonpath = 'hello'
backlog = 10240000