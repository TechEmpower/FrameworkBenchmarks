import multiprocessing
import os

_is_travis = os.environ.get('TRAVIS') == 'true'

workers = multiprocessing.cpu_count() * 2
if _is_travis:
    workers = 2

bind = ['0.0.0.0:8008', '0.0.0.0:8009', '0.0.0.0:8011']
keepalive = 120
errorlog = '-'
pidfile = 'api_hour.pid'
pythonpath = 'hello'
backlog = 10240000