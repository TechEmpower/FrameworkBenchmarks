FROM pypy:3.9-bullseye

RUN apt-get update
RUN apt-get install libpq-dev python3-dev libuv1 -y

WORKDIR /flask
COPY ./ /flask
RUN pip3 install -U pip; pip3 install -r /flask/requirements-socketify.txt

EXPOSE 8080

CMD python ./app-socketify-wsgi.py
