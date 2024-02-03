FROM python:3.11-bullseye

WORKDIR /panther

COPY ./ /panther

RUN pip3 install -U pip
RUN pip3 install -r /panther/requirements.txt

EXPOSE 8080

CMD gunicorn app:app -k uvicorn.workers.UvicornWorker -c panther_conf.py
