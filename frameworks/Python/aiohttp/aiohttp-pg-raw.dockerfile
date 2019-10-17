FROM python:3.8

ADD ./ /aiohttp

WORKDIR aiohttp

RUN pip3 install cython==0.29.13 && \
    pip3 install -r /aiohttp/requirements.txt

ENV CONNECTION=RAW

CMD gunicorn app.gunicorn:app -c gunicorn_conf.py
