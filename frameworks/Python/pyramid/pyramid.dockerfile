FROM python:3.14

ADD ./requirements.txt /pyramid/requirements.txt

RUN pip3 install -r /pyramid/requirements.txt
ADD ./ /pyramid
WORKDIR /pyramid

EXPOSE 8080

CMD gunicorn wsgi:app -c gunicorn_conf.py
