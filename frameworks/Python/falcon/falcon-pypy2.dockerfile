FROM pypy:2-5.10

ADD ./ /falcon

WORKDIR /falcon

RUN pip install -r /falcon/requirements-pypy.lock

CMD gunicorn app:app -c gunicorn_conf.py
