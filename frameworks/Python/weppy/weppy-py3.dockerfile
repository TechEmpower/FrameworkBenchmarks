FROM techempower/python3:0.1

ADD ./ /weppy

WORKDIR /weppy

RUN pip3 install --install-option="--prefix=${PY3_ROOT}" -r /weppy/requirements.txt

CMD gunicorn app:app -c gunicorn_conf.py
