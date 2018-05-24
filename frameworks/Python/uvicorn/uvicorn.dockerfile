FROM python:3.6.5

ADD ./ /uvicorn

WORKDIR /uvicorn

RUN pip3 install -r /uvicorn/requirements.txt

CMD uvicorn app:main -c uvicorn_conf.py
