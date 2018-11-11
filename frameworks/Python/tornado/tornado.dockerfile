FROM python:2.7.15-stretch

ADD ./ /tornado

WORKDIR /tornado

RUN pip install -r /tornado/requirements_mongo.txt

CMD python server_py2.py --port=8080 --mongo=tfb-database --logging=error
