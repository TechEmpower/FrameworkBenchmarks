FROM tfb/python3:latest

ADD ./aiohttp.web /aiohttp.web
ADD ./requirements.txt /aiohttp.web

WORKDIR /aiohttp.web

RUN pip3 install --install-option="--prefix=${PY3_ROOT}" -r /aiohttp.web/requirements.txt
