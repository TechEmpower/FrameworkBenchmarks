FROM tfb/aiohttp-base:latest

WORKDIR /aiohttp

ENV CONNECTION=RAW

CMD gunicorn app.gunicorn:app -c gunicorn_conf.py
