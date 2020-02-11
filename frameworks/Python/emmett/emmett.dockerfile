FROM python:3.7-alpine

RUN apk add --no-cache libpq libstdc++

RUN mkdir -p /usr/src/app
WORKDIR /usr/src/app
COPY requirements.txt /usr/src/app
RUN apk add --no-cache --virtual build-deps \
    g++ libffi-dev libuv-dev make musl-dev openssl-dev postgresql-dev && \
    pip install --no-cache-dir -r /usr/src/app/requirements.txt && \
    apk del build-deps

COPY ./ /app
WORKDIR /app

EXPOSE 8080

CMD [ "gunicorn", "app:app" , "-k", "emmett.asgi.workers.EmmettWorker", "-c", "gunicorn_conf.py" ]
