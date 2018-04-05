FROM techempower/flask-py3-base:0.1

WORKDIR /flask

CMD gunicorn app:app -c gunicorn_conf.py
