FROM python:3.6.6-stretch

ADD ./ /responder

WORKDIR /responder

RUN pip3 install -r /responder/requirements.txt

CMD gunicorn app:app -k uvicorn.workers.UvicornWorker -c responder_conf.py
