FROM python:2.7.14

ADD ./ /tornado

WORKDIR /tornado

RUN pip install -r /tornado/requirements_pg.txt

CMD python server_pg.py --port=8080 --postgres=tfb-database --logging=error
