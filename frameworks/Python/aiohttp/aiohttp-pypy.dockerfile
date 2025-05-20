FROM pypy:3.13

ADD ./ /aiohttp

WORKDIR /aiohttp

RUN pip3 install -r /aiohttp/requirements.txt

ENV CONNECTION=RAW

EXPOSE 8080

CMD python3 -O -m app.server
