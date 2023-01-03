FROM python:3.10

ADD ./ /bareasgi

WORKDIR /bareasgi

RUN pip install -r /bareasgi/requirements.txt

EXPOSE 8080

CMD hypercorn app:app --config=file:hypercorn_conf.py
