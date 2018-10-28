FROM python:3.6.6-stretch

ADD ./ /wheezyweb

WORKDIR /wheezyweb

RUN pip3 install -r /wheezyweb/requirements.txt

CMD gunicorn app:app -c gunicorn_conf.py
