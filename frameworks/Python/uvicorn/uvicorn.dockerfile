FROM python:3.8

ADD ./ /uvicorn

WORKDIR /uvicorn

RUN pip3 install cython==0.29.13 && \
    pip3 install -r /uvicorn/requirements.txt

EXPOSE 8080

CMD gunicorn app:main -k uvicorn.workers.UvicornWorker -c uvicorn_conf.py
