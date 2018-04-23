FROM python:2.7.14

ADD ./ /falcon

WORKDIR /falcon

RUN pip install -r /falcon/requirements.lock

CMD gunicorn app:app -c gunicorn_conf.py
