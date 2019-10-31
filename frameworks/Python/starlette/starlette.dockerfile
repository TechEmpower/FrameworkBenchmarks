FROM python:3.8

ADD ./ /starlette

WORKDIR /starlette

RUN pip3 install cython==0.29.13 && \
    pip3 install -r /starlette/requirements.txt

CMD gunicorn app:app -k uvicorn.workers.UvicornWorker -c starlette_conf.py
