FROM python:3.6.6-stretch

ADD ./ /pyramid

WORKDIR /pyramid

RUN pip3 install -r /pyramid/requirements_mongo.txt

CMD python3 server_py3.py --port=8080 --mongo=tfb-database --logging=error
