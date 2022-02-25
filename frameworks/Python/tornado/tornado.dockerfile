FROM python:2.7.15-stretch

ADD ./ /tornado

WORKDIR /tornado

RUN pip install -r /tornado/requirements_py2_mongo.txt

EXPOSE 8080

CMD python server_py2.py --port=8080 --mongo=tfb-database --logging=error
