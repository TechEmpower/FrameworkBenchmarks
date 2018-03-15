FROM tfb/python2:latest

ADD ./ /weppy

WORKDIR /weppy

RUN pip install --install-option="--prefix=${PY2_ROOT}" -r /weppy/requirements.txt

CMD gunicorn app:app -c gunicorn_conf.py
