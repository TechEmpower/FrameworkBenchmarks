FROM pypy:2-5.10

ADD ./ /flask

WORKDIR /flask

RUN pip install -r /flask/requirements-pypy.txt

WORKDIR /flask

CMD gunicorn app:app -c gunicorn_conf.py
