FROM tfb/python2:latest

ADD ./ /wheezyweb

WORKDIR /wheezyweb

RUN pip install --install-option="--prefix=${PY2_ROOT}" -r /wheezyweb/requirements.txt

CMD gunicorn app:app -c gunicorn_conf.py
