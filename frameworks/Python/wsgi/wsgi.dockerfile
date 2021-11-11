FROM python:3.6.6-stretch

ADD ./ /wsgi

WORKDIR /wsgi

RUN pip3 install -r /wsgi/requirements.txt

EXPOSE 8080

CMD gunicorn hello:app -c gunicorn_conf.py
