FROM techempower/aiohttp-base:0.1

WORKDIR /aiohttp

ENV CONNECTION=RAW

CMD gunicorn app.gunicorn:app -c gunicorn_conf.py
