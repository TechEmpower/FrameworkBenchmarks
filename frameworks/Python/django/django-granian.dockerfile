FROM python:3.14

ADD ./ /django
WORKDIR /django
RUN pip install -r /django/requirements-granian.txt

ENV PYTHONPATH=/django/hello
ENV DJANGO_DB=postgresql

EXPOSE 8080

# Use WSGI mode - Django's sync ORM crashes workers in ASGI mode under load
# runtime-mode=mt for multi-threaded (same as TechEmpower)
CMD granian --interface wsgi hello.wsgi:application --host 0.0.0.0 --port 8080 --workers $(nproc) --backlog 16384 --blocking-threads 1 --runtime-mode mt
