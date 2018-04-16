FROM python:3.6.5

ADD ./ /wheezyweb

WORKDIR /wheezyweb

RUN pip3 install -r /wheezyweb/requirements.txt

CMD gunicorn app:app -c gunicorn_conf.py
