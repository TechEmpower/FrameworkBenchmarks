FROM techempower/python2:0.1

ADD ./ /tornado

WORKDIR /tornado

RUN pip install --install-option="--prefix=${PY2_ROOT}" -r /tornado/requirements_mongo.txt

CMD python server_py2.py --port=8080 --mongo=$DBHOST --logging=error
