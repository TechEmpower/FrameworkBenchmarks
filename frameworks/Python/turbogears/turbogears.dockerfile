FROM techempower/python2:0.1

ADD ./ /turbogears

WORKDIR /turbogears

RUN pip install --install-option="--prefix=${PY2_ROOT}" -r /turbogears/requirements.txt

CMD gunicorn app:app -c gunicorn_conf.py
