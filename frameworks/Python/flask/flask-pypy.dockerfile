FROM pypy:3

RUN apt-get update
RUN apt-get install libpq-dev python3-dev -y
ADD ./requirements-pypy.txt /flask/requirements-pypy.txt
RUN pip3 install -r /flask/requirements-pypy.txt
ADD ./ /flask
WORKDIR /flask

EXPOSE 8080

CMD gunicorn app:app -c gunicorn_conf.py
