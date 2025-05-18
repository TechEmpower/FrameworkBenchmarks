FROM pypy:3.11

ADD ./ /aiohttp

WORKDIR /aiohttp

RUN pip3 install cython==3.0.11 && \
    pip3 install -r /aiohttp/requirements.txt

ENV CONNECTION=RAW

EXPOSE 8080

CMD python3 -O -m app.server
