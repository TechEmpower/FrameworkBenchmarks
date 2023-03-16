FROM python:3.10-bullseye


RUN apt-get update
RUN apt-get install libpq-dev python3-dev -y
ADD ./requirements-fastwsgi.txt /flask/requirements.txt
RUN pip3 install -r /flask/requirements.txt
ADD ./ /flask
WORKDIR /flask

EXPOSE 8080

CMD python ./app-fastwsgi.py
