FROM python:3.10-bullseye

RUN mkdir -p /usr/src/app
WORKDIR /usr/src/app
COPY requirements-socketify.txt /usr/src/app
RUN apt-get update; apt-get install libuv1 -y
RUN pip install --no-cache-dir -r /usr/src/app/requirements-socketify.txt

COPY ./ /app
WORKDIR /app

EXPOSE 8080

CMD python ./app-socketify-asgi.py
