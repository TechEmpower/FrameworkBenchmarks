FROM python:3.11-bullseye


RUN apt-get update
RUN apt-get install libpq-dev python3-dev -y

ADD ./requirements-fastwsgi.txt /flask/

WORKDIR /flask

RUN pip3 install -r /flask/requirements-fastwsgi.txt

ADD ./ /flask

EXPOSE 8080

CMD python ./app-fastwsgi.py
