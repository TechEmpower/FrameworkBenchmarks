FROM python:3.13

ADD ./ /aiohttp

WORKDIR aiohttp

RUN pip3 install cython==3.0.11 && \
    pip3 install -r /aiohttp/requirements.txt

WORKDIR /aiohttp

EXPOSE 8080

CMD python3 -O -m gunicorn app.gunicorn:app -c gunicorn_conf.py
