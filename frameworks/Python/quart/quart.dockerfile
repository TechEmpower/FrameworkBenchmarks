FROM python:3.8

ADD ./ /quart

WORKDIR /quart

RUN pip3 install -r /quart/requirements.txt

CMD hypercorn app:app --config=python:hypercorn_conf.py
