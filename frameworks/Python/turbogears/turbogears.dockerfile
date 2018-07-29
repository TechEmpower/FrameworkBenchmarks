FROM python:2.7.14

ADD ./ /turbogears

WORKDIR /turbogears

RUN pip install -r /turbogears/requirements.txt

CMD gunicorn app:app -c gunicorn_conf.py
