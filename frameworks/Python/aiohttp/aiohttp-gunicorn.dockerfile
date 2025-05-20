FROM python:3.13

ADD ./ /aiohttp

WORKDIR aiohttp

RUN pip3 install -r /aiohttp/requirements-cpython.txt

ENV CONNECTION=RAW

EXPOSE 8080

CMD python3 -O -m gunicorn app.gunicorn:app -c gunicorn_conf.py
