FROM python:3.10-rc-slim-buster

ADD ./ /spyne

WORKDIR /spyne

RUN pip3 install -r /spyne/requirements.txt

CMD gunicorn app:application -c gunicorn_conf.py
