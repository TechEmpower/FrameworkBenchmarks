FROM techempower/flask-pypy2-base:0.1

WORKDIR /flask

CMD gunicorn app:app -c gunicorn_conf.py
