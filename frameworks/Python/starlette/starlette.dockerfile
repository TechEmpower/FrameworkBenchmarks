FROM python:3.10

ADD ./ /starlette

WORKDIR /starlette

RUN pip install -r /starlette/requirements.txt

EXPOSE 8080

CMD gunicorn app:app -k uvicorn.workers.UvicornWorker -c starlette_conf.py
