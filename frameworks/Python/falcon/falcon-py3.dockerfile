FROM python:3.6.5

ADD ./ /falcon

WORKDIR /falcon

RUN pip3 install -r /falcon/requirements.lock

CMD gunicorn app:app -c gunicorn_conf.py
