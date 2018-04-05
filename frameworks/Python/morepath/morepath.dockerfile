FROM techempower/python3:0.1

ADD ./ /japronto

WORKDIR /japronto

RUN pip3 install --install-option="--prefix=${PY3_ROOT}" -r /japronto/requirements.txt

CMD gunicorn app.run -c gunicorn_conf.py
