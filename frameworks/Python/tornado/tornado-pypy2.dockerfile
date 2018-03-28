FROM tfb/pypy2:latest

ADD ./ /tornado

WORKDIR /tornado

RUN pip install --install-option="--prefix=${PYPY2_ROOT}" -r /tornado/requirements_mongo.txt

CMD pypy server_py2.py --port=8080 --mongo=$DBHOST --logging=error
