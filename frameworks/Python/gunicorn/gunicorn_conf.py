import multiprocessing

workers = multiprocessing.cpu_count()
bind = "0.0.0.0:8080"
keepalive = 60
errorlog = '-'
pidfile = 'gunicorn.pid'
worker_class = "meinheld.gmeinheld.MeinheldWorker"
