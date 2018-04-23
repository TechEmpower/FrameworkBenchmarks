FROM python:3.6.5

ADD ./ /bottle

WORKDIR /bottle

RUN pip3 install -r /bottle/requirements.txt

CMD gunicorn app:app -c gunicorn_conf.py
