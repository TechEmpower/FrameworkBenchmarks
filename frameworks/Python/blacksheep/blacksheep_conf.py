import multiprocessing
import os

_is_travis = os.environ.get('TRAVIS') == 'true'
CPU_CORES = multiprocessing.cpu_count()
MAX_CONNECTIONS = 1900 
CONNECTIONS_PER_WORKER = 100
max_pg_workers = MAX_CONNECTIONS // CONNECTIONS_PER_WORKER

workers = CPU_CORES
if _is_travis:
    workers = 2

bind = "0.0.0.0:8080"
keepalive = 1
timeout = 0
errorlog = '-'
pidfile = '/tmp/blacksheep.pid'
loglevel = 'error'
