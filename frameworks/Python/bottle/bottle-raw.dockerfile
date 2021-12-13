FROM python:3.6.6-stretch

WORKDIR /bottle
COPY views views
COPY app.py app.py
COPY gunicorn_conf.py gunicorn_conf.py
COPY requirements.txt requirements.txt

RUN pip3 install -r requirements.txt

EXPOSE 8080

CMD gunicorn app:app -c gunicorn_conf.py
