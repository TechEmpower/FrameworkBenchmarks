FROM python:3.13

ADD ./ /aiohttp

WORKDIR /aiohttp

RUN pip3 install -r /aiohttp/requirements-cpython.txt && \
    pip3 install cython==3.1.2 setuptools==80.9.0 && \
    python3 setup.py build_ext --inplace

ENV CONNECTION=RAW

EXPOSE 8080

CMD python3 -O -m app.server
