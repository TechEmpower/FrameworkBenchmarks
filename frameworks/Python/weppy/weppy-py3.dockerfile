FROM python:3.6.5

ADD ./ /weppy

WORKDIR /weppy

RUN pip3 install -r /weppy/requirements.txt

CMD gunicorn app:app -c gunicorn_conf.py
