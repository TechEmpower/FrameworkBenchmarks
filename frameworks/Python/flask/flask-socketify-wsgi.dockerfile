FROM python:3.10-bullseye


RUN apt-get update
RUN apt-get install libpq-dev python3-dev libuv1 -y
ADD ./requirements-socketify.txt /flask/requirements.txt
RUN pip3 install -r /flask/requirements.txt
ADD ./ /flask
WORKDIR /flask

EXPOSE 8080

CMD python ./app-socketify-wsgi.py
