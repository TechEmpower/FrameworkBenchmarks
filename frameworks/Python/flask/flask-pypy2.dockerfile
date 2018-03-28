FROM tfb/flask-pypy2-base:latest

WORKDIR /flask

CMD gunicorn app:app -c gunicorn_conf.py
