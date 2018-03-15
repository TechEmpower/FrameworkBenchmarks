FROM tfb/python3:latest

ADD ./ /wheezyweb

WORKDIR /wheezyweb

RUN pip3 install --install-option="--prefix=${PY3_ROOT}" -r /wheezyweb/requirements.txt

CMD gunicorn app:app -c gunicorn_conf.py
