FROM python:3.6.6-stretch

ADD ./ /apistar

WORKDIR /apistar

RUN pip3 install -r /apistar/requirements.txt

CMD gunicorn app:app -c gunicorn_conf.py
