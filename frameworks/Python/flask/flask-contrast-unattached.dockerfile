FROM python:3.8-buster


RUN apt-get update
RUN apt-get install libpq-dev python3-dev -y
ADD ./requirements.txt /flask/requirements.txt
RUN pip3 install -r /flask/requirements.txt
ADD ./ /flask
WORKDIR /flask

EXPOSE 8080

CMD gunicorn app:app -c gunicorn_conf.py
