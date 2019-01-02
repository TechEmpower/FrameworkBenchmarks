FROM python:3.6.6-stretch

ADD ./ /fastapi

WORKDIR /fastapi

RUN pip3 install -r /fastapi/requirements.txt

CMD gunicorn app:app -k uvicorn.workers.UvicornWorker -c fastapi_conf.py
