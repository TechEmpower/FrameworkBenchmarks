import multiprocessing
import os
import sys

workers = multiprocessing.cpu_count() * 3

bind = "0.0.0.0:8080"
keepalive = 120
errorlog = '-'
pidfile = 'gunicorn.pid'
