FROM python:3.6.6-stretch

ADD ./ /weppy

WORKDIR /weppy

RUN pip3 install -r /weppy/requirements.txt

EXPOSE 8080

CMD gunicorn app:app -c gunicorn_conf.py
