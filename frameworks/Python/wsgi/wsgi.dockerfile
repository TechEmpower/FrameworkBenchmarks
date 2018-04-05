FROM techempower/python3:0.1

ADD ./ /wsgi

WORKDIR /wsgi

RUN pip3 install --install-option="--prefix=${PY3_ROOT}" -r /wsgi/requirements.txt

CMD gunicorn hello:app -c gunicorn_conf.py
