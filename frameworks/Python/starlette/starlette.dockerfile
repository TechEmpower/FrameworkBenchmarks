FROM python:3.6.6-stretch

ADD ./ /starlette

WORKDIR /starlette

RUN pip3 install -r /starlette/requirements.txt

CMD gunicorn app:app -k uvicorn.workers.UvicornWorker -c starlette_conf.py
