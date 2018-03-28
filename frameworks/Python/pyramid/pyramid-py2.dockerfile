FROM techempower/python2:0.1

ADD ./ /pyramid

WORKDIR /pyramid

RUN pip install --install-option="--prefix=${PY2_ROOT}" -r /pyramid/requirements.txt

CMD gunicorn wsgi:app -c gunicorn_conf.py
