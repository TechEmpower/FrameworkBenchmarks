FROM tfb/python3:latest

ADD ./ /apistar

WORKDIR /apistar

RUN pip3 install --install-option="--prefix=${PY3_ROOT}" -r /apistar/requirements.txt

CMD gunicorn app:app.wsgi -c gunicorn_conf.py
