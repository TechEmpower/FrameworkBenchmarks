FROM techempower/python3:0.1

ADD ./ /pyramid

WORKDIR /pyramid

RUN pip3 install --install-option="--prefix=${PY3_ROOT}" -r /pyramid/requirements_mongo.txt

CMD python3 server_py3.py --port=8080 --mongo=$DBHOST --logging=error
