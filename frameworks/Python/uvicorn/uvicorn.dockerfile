FROM python:3.6.6-stretch

ADD ./ /uvicorn

WORKDIR /uvicorn

RUN pip3 install -r /uvicorn/requirements.txt

CMD gunicorn app:main -k uvicorn.workers.UvicornWorker -c uvicorn_conf.py
