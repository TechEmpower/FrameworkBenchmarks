FROM python:3.11-bullseye


RUN apt-get update
RUN apt-get install libpq-dev python3-dev -y

WORKDIR /flask
COPY ./ /flask
RUN pip3 install -U pip; pip3 install -r /flask/requirements-fastwsgi.txt 

EXPOSE 8080

ENV USE_UJSON=1
ENV USE_RAW=1

CMD python ./app.py -s fastwsgi
