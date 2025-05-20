FROM python:3.13

ADD ./ /aiohttp

WORKDIR /aiohttp

RUN pip3 install -r /aiohttp/requirements-cpython.txt

ENV CONNECTION=ORM

EXPOSE 8080

CMD python3 -O -m app.server
