FROM python:3.8

ADD ./ /fastapi

WORKDIR /fastapi

RUN pip3 install -r /fastapi/requirements-orjson.txt

EXPOSE 8080

CMD gunicorn app_orjson:app -k uvicorn.workers.UvicornWorker -c fastapi_conf.py
