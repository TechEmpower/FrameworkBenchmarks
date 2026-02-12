import multiprocessing

workers = multiprocessing.cpu_count()
bind = "0.0.0.0:8080"
keepalive = 120
errorlog = "-"
accesslog = None
pidfile = "/tmp/gunicorn.pid"
worker_class = "sync"
