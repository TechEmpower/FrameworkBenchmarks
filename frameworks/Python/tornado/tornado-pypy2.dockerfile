FROM pypy:2-5.10

ADD ./ /tornado

WORKDIR /tornado

RUN pip install -r /tornado/requirements_py2_mongo.txt

EXPOSE 8080

CMD pypy server_py2.py --port=8080 --mongo=tfb-database --logging=error
