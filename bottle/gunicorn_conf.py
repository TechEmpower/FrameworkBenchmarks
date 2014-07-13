import multiprocessing

workers = multiprocessing.cpu_count() * 3
bind = "0.0.0.0:8080"
worker_class = "meinheld.gmeinheld.MeinheldWorker"
keepalive = 120

def post_fork(server, worker):
    # Disalbe access log
    import meinheld.server
    meinheld.server.set_access_logger(None)
