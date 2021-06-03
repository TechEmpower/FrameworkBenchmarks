FROM python:3.6.6-stretch

ADD ./ /flask

WORKDIR /flask

RUN pip3 install -r /flask/requirements.txt

EXPOSE 8080

CMD gunicorn app:app -c gunicorn_conf.py
