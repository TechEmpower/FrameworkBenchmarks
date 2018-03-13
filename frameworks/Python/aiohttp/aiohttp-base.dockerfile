FROM tfb/python3:latest

ADD ./ /aiohttp

WORKDIR aiohttp

RUN pip3 install --install-option="--prefix=${PY3_ROOT}" -r /aiohttp/requirements.txt

CMD gunicorn app.gunicorn:app -c gunicorn_conf.py
