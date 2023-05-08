FROM python:3.10-bullseye

ADD ./ /django

WORKDIR /django
RUN apt-get update; apt-get install libuv1 -y
RUN pip install -r /django/requirements-socketify.txt

EXPOSE 8080

CMD python ./django-socketify-wsgi.py
