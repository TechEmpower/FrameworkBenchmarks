FROM python:3.8

RUN mkdir -p /usr/src/app
WORKDIR /usr/src/app
COPY requirements.txt /usr/src/app
RUN pip install --no-cache-dir -r /usr/src/app/requirements.txt

COPY ./ /app
WORKDIR /app

EXPOSE 8080

CMD [ "gunicorn", "app:app" , "-k", "emmett.asgi.workers.EmmettWorker", "-c", "gunicorn_conf.py" ]
