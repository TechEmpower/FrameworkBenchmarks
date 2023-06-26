FROM python:3.9-bullseye


RUN apt-get update
RUN apt-get install libpq-dev python3-dev -y

WORKDIR /flask
COPY ./ /flask
RUN pip3 install -U pip; pip3 install -r /flask/requirements-gunicorn.txt 

EXPOSE 8080

CMD gunicorn app:app -c gunicorn_conf.py
