FROM techempower/python2:0.1

ADD ./ /tornado

WORKDIR /tornado

RUN pip install --install-option="--prefix=${PY2_ROOT}" -r /tornado/requirements_pg.txt

CMD python server_pg.py --port=8080 --postgres=$DBHOST --logging=error
