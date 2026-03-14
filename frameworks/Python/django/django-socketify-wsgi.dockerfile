FROM python:3.14

ADD ./ /django

WORKDIR /django

RUN apt-get update && \
    apt-get install gcc g++ make libuv1 zlib1g -y && \
    pip install -r /django/requirements-socketify.txt

EXPOSE 8080

CMD python ./django-socketify-wsgi.py
