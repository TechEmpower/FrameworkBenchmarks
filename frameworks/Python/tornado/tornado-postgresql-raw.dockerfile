FROM tfb/python2:latest

ADD ./ /tornado

WORKDIR /tornado

RUN pip install --install-option="--prefix=${PY2_ROOT}" -r /tornado/requirements_pg.txt

CMD python server_pg.py --port=8080 --postgres=$DBHOST --logging=error
