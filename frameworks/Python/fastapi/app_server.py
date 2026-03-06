#!/usr/bin/env python
import os
import sys
import fastapi

from app import app

if __name__ == "__main__":
    import optparse
    import multiprocessing
    import logging

    parser = optparse.OptionParser("usage: %prog [options]", add_help_option=False)
    parser.add_option("-h", "--host", dest="host", default='0.0.0.0', type="string")
    parser.add_option("-p", "--port", dest="port", default=8080, type="int")
    parser.add_option("-s", "--server", dest="server", default="", type="string")
    parser.add_option("-w", "--workers", dest="workers", default=0, type="int")
    parser.add_option("-k", "--keepalive", dest="keepalive", default=60, type="int")
    parser.add_option("-v", "--verbose", dest="verbose", default=0, type="int")
    (opt, args) = parser.parse_args() 

    _is_travis = os.environ.get('TRAVIS') == 'true'

    workers = opt.workers
    if workers <= 0:
        workers = int(multiprocessing.cpu_count())

    if _is_travis:
        workers = 2

    def run_app():
        if opt.server in [ 'si', 'socketify' ]:
            import socketify
            siapp = socketify.ASGI(app)
            siapp.listen(opt.port, lambda config: logging.info(f"Listening on port http://localhost:{opt.port} now\n"))
            siapp.run()
            return

        if opt.server in [ 'fp', 'fastpysgi' ]:
            import fastpysgi
            fastpysgi.server.loop_timeout = 1
            fastpysgi.server.hook_sigint = 1
            fastpysgi.server.backlog = 4096
            fastpysgi.run(app, host=opt.host, port=opt.port, loglevel=opt.verbose)
            return

        raise Exception(f'Unknown server name = "{opt.server}"')

    def create_fork():
        n = os.fork()
        # n greater than 0 means parent process
        if not n > 0:
            run_app()

    # fork limiting the cpu count - 1
    for i in range(1, workers):
        create_fork()

    run_app()  # run app on the main process too :) 
