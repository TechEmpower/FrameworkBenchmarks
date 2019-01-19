import multiprocessing
import os

if os.environ.get('TRAVIS') == 'true':
    workers = 2
else:
    workers = multiprocessing.cpu_count() * 3

bind = '0.0.0.0:8080'
keepalive = 120
errorlog = '-'
pidfile = 'gunicorn.pid'

worker_class = "meinheld.gmeinheld.MeinheldWorker"
