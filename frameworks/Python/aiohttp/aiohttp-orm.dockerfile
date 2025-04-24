FROM python:3.13

ADD ./requirements.txt /aiohttp/requirements.txt

RUN pip3 install cython==3.0.11 && \
    pip3 install -r /aiohttp/requirements.txt

ADD ./ /aiohttp

WORKDIR /aiohttp

ENV CONNECTION=ORM

EXPOSE 8080

CMD python3 -O -m app.server