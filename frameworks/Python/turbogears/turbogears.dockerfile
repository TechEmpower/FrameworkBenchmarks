FROM tfb/python2:latest

ADD ./ /turbogears

WORKDIR /turbogears

RUN pip install --install-option="--prefix=${PY2_ROOT}" -r /turbogears/requirements.txt

CMD gunicorn app:app -c gunicorn_conf.py
