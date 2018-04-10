FROM python:3.6.5

ADD ./ /flask

WORKDIR /flask

RUN pip3 install -r /flask/requirements.txt

WORKDIR /flask

CMD gunicorn app:app -c gunicorn_conf.py
