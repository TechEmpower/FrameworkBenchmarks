FROM pypy:2-5.10

ADD ./ /tornado

WORKDIR /tornado

RUN pip install -r /tornado/requirements_mongo.txt

CMD pypy server_py2.py --port=8080 --mongo=tfb-database --logging=error
