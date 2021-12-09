FROM python:3.9

ADD ./ /pyramid

WORKDIR /pyramid

RUN pip3 install -r /pyramid/requirements.txt

EXPOSE 8080

CMD gunicorn wsgi:app -c gunicorn_conf.py
