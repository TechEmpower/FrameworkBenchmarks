FROM python:3.6.5

ADD ./ /starlette

WORKDIR /starlette

RUN pip3 install -r /starlette/requirements.txt

CMD gunicorn app:main -k uvicorn.workers.UvicornWorker -c starlette_conf.py
