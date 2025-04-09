import uvloop
from aiohttp import web
import argparse
from .main import create_app


if __name__ == '__main__':
    uvloop.install()
    parser = argparse.ArgumentParser()
    parser.add_argument('--port', type=int, required=True)
    args = parser.parse_args()

    app = create_app()
    web.run_app(app, port=args.port)