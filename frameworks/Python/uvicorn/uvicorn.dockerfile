FROM python:3.11

ADD ./ /uvicorn

WORKDIR /uvicorn

RUN pip3 install cython==0.29.36 && \
    pip3 install -r /uvicorn/requirements.txt

EXPOSE 8080

CMD gunicorn app:main -k uvicorn.workers.UvicornWorker -c uvicorn_conf.py
