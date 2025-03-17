FROM python:3.9

ADD ./ /aiohttp

WORKDIR aiohttp

RUN pip3 install cython==3.0.11 && \
    pip3 install -r /aiohttp/requirements.txt

ENV CONNECTION=RAW

EXPOSE 8080

CMD python3 -O -m gunicorn app.gunicorn:app -c gunicorn_conf.py
