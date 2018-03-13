FROM tfb/python3:latest

ADD ./ /falcon

WORKDIR /falcon

RUN pip3 install --install-option="--prefix=${PY3_ROOT}" -r /falcon/requirements.lock

CMD gunicorn app:app -c gunicorn_conf.py
