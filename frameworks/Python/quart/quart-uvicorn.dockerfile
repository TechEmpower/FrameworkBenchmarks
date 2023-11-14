FROM python:3.10

ADD ./ /quart

WORKDIR /quart

RUN pip3 install -r /quart/requirements.txt
RUN pip3 install -r /quart/requirements-uvicorn.txt

EXPOSE 8080

CMD gunicorn app:app -k uvicorn.workers.UvicornWorker -c gunicorn_conf.py
