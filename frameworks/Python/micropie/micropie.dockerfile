FROM python:3.10

ADD ./ /micropie

WORKDIR /micropie

RUN pip install -r /micropie/requirements.txt

EXPOSE 8080

CMD gunicorn app:app -k uvicorn.workers.UvicornWorker -c micropie_conf.py
