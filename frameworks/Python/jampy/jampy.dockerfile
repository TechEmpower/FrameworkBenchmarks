FROM python:3.6.6-stretch

ADD ./ /jampy

WORKDIR /jampy

RUN pip3 install -r /jampy/requirements.txt

RUN git clone https://github.com/jam-py/jam-py.git

WORKDIR /jam-py/demo

EXPOSE 8080

CMD gunicorn wsgi -c gunicorn_conf.py
