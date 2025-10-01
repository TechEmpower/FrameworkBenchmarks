FROM pypy:3.11-slim

ADD ./ /django

WORKDIR /django
RUN apt-get update; apt-get install libuv1 zlib1g -y
RUN pip install -r /django/requirements-socketify.txt

EXPOSE 8080

CMD python ./django-socketify-wsgi.py
