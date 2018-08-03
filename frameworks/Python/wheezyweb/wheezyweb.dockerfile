FROM python:2.7.14

ADD ./ /wheezyweb

WORKDIR /wheezyweb

RUN pip install -r /wheezyweb/requirements.txt

CMD gunicorn app:app -c gunicorn_conf.py
