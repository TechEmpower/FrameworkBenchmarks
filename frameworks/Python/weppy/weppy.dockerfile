FROM techempower/python2:0.1

ADD ./ /weppy

WORKDIR /weppy

RUN pip install --install-option="--prefix=${PY2_ROOT}" -r /weppy/requirements.txt

CMD gunicorn app:app -c gunicorn_conf.py
