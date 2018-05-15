FROM python:3.6.5

ADD ./ /apistar

WORKDIR /apistar

RUN pip3 install -r /apistar/requirements.txt

CMD gunicorn app:app.wsgi -c gunicorn_conf.py
