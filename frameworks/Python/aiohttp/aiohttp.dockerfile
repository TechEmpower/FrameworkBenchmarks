FROM python:3.6.5

ADD ./ /aiohttp

WORKDIR aiohttp

RUN pip3 install -r /aiohttp/requirements.txt

WORKDIR /aiohttp

CMD gunicorn app.gunicorn:app -c gunicorn_conf.py
