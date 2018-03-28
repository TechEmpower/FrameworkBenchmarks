FROM techempower/python3:0.1

ADD ./ /pyramid

WORKDIR /pyramid

RUN pip3 install --install-option="--prefix=${PY3_ROOT}" -r /pyramid/requirements.txt

CMD gunicorn wsgi:app -c gunicorn_conf.py
