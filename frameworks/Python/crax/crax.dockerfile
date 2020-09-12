FROM python:3.8

ADD ./ /crax

WORKDIR /crax

RUN pip3 install cython==0.29.13 && \
    pip3 install -r /crax/requirements.txt

CMD gunicorn hello.app:app -k uvicorn.workers.UvicornWorker -c crax_conf.py