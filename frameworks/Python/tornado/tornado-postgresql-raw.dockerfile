FROM python:2.7.15-stretch

ADD ./ /tornado

WORKDIR /tornado

RUN pip install -r /tornado/requirements_py2_pg.txt

EXPOSE 8080

CMD python server_pg.py --port=8080 --postgres=tfb-database --logging=error
