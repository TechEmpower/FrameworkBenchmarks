import argparse
import platform

from aiohttp import web

from .main import create_app


if __name__ == '__main__':
    if platform.python_implementation() != "PyPy":
        import uvloop
        uvloop.install()
    parser = argparse.ArgumentParser()
    parser.add_argument('--socket', type=str, required=True)
    args = parser.parse_args()

    app = create_app()
    web.run_app(app, path=args.socket, access_log=None)
