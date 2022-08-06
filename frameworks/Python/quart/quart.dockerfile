FROM python:3.10

ADD ./ /quart

WORKDIR /quart

RUN pip3 install -r /quart/requirements.txt

EXPOSE 8080

CMD hypercorn app:app --config=file:hypercorn_conf.py
