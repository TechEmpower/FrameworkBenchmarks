FROM tfb/aiohttp-base:latest

WORKDIR /aiohttp

CMD gunicorn app.gunicorn:app -c gunicorn_conf.py
