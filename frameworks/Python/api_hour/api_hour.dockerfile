FROM python:3.6.6-stretch

ADD ./aiohttp.web /aiohttp.web
ADD ./requirements.txt /aiohttp.web

WORKDIR /aiohttp.web

RUN pip3 install -r /aiohttp.web/requirements.txt

WORKDIR /aiohttp.web

CMD api_hour -ac hello:Container
