FROM pypy:2-5.10

ADD ./ /bottle

WORKDIR /bottle

RUN pip install -r /bottle/requirements-pypy.txt

CMD gunicorn app:app -c gunicorn_conf.py
