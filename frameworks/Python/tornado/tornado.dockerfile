FROM python:3.6.6-stretch

ADD ./ /tornado

WORKDIR /tornado

RUN pip3 install -r /tornado/requirements_mongo.txt

EXPOSE 8080

CMD python3 server.py --port=8080 --mongo=tfb-database --logging=error
