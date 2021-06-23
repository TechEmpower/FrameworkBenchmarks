FROM python:3.6.6-stretch

ADD ./ /jampy

WORKDIR /jampy

RUN pip3 install -r /jampy/requirements.txt

RUN cp gunicorn_conf.py /jampy/app/

WORKDIR /jampy/app

EXPOSE 8080

CMD gunicorn wsgi -c gunicorn_conf.py
