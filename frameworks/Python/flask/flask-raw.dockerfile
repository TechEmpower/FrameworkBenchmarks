FROM tfb/flask-py3-base:latest

WORKDIR /flask

CMD gunicorn app:app -c gunicorn_conf.py
