FROM python:2.7.15-stretch

ADD ./ /weppy

WORKDIR /weppy

RUN pip install -r /weppy/requirements.txt

CMD gunicorn app:app -c gunicorn_conf.py
