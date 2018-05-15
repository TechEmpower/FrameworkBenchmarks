FROM python:2.7.14

ADD ./ /pyramid

WORKDIR /pyramid

RUN pip install -r /pyramid/requirements.txt

CMD gunicorn wsgi:app -c gunicorn_conf.py
