FROM tfb/python3:latest

ADD ./ /bottle

WORKDIR /bottle

RUN pip3 install --install-option="--prefix=${PY3_ROOT}" -r /bottle/requirements.txt

CMD gunicorn app:app -c gunicorn_conf.py
