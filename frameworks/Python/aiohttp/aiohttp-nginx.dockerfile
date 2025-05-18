FROM python:latest

RUN apt-get update && apt-get install -y nginx

ADD ./ /aiohttp

WORKDIR /aiohttp

RUN pip3 install cython==3.0.11 && \
    pip3 install -r /aiohttp/requirements-cpython.txt

ENV CONNECTION=RAW

EXPOSE 8080

RUN chmod +x /aiohttp/nginx-entrypoint.sh

ENTRYPOINT ["/aiohttp/nginx-entrypoint.sh"]
