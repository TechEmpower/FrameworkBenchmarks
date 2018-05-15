FROM python:3.6.5

ADD ./ /aiohttp

WORKDIR aiohttp

RUN pip3 install -r /aiohttp/requirements.txt

ENV CONNECTION=RAW

CMD gunicorn app.gunicorn:app -c gunicorn_conf.py
