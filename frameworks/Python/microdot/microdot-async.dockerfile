FROM python:3.11-buster

RUN apt-get update
RUN apt-get install libpq-dev python3-dev -y
ADD ./requirements.txt /microdot/requirements.txt
RUN pip3 install -r /microdot/requirements.txt
ADD ./ /microdot
WORKDIR /microdot

ENV PYTHONUNBUFFERED 1
ENV DATABASE_URL postgresql://benchmarkdbuser:benchmarkdbpass@tfb-database:5432/hello_world

EXPOSE 8080

CMD gunicorn app_async:app -c uvicorn_conf.py
