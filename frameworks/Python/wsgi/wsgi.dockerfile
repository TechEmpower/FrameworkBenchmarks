FROM python:3.6.6-stretch

ADD ./ /wsgi

WORKDIR /wsgi

RUN pip3 install -r /wsgi/requirements.txt

CMD gunicorn hello:app -c gunicorn_conf.py
