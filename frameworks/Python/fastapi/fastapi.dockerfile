FROM python:3.8

ADD ./ /fastapi

WORKDIR /fastapi

RUN pip3 install cython==0.29.13 && \
    pip3 install -r /fastapi/requirements.txt

CMD gunicorn app:app -k uvicorn.workers.UvicornWorker -c fastapi_conf.py
